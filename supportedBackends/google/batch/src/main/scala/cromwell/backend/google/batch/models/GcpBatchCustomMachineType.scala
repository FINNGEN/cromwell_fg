package cromwell.backend.google.batch.models

import cromwell.backend.google.batch.models.CustomMachineType._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import mouse.all._
import org.slf4j.Logger
import wdl4s.parser.MemoryUnit
import wom.format.MemorySize

import scala.math.{log, pow}

/**
  * Adjusts memory and cpu for custom machine types.
  *
  * For more info see:
  * - https://cloud.google.com/compute/docs/instances/creating-instance-with-custom-machine-type
  * - https://cloud.google.com/compute/docs/instances/creating-instance-with-custom-machine-type#specifications
  * - https://cloud.google.com/compute/docs/instances/creating-instance-with-custom-machine-type#gcloud
  * - https://cloud.google.com/sdk/gcloud/reference/compute/instances/create#--custom-vm-type
  */
trait CustomMachineType {

  /**
    * The machine family name as accepted by the `machineFamily` runtime attribute, e.g. "n2d".
    */
  def family: String

  /**
    * The vm prefix to create this custom machine type.
    */
  def vmTypePrefix: String

  /**
    * Whether this family supports extended memory, i.e. memory beyond `maxMemoryPerCpu` billed at the extended
    * memory rate, requested by appending `-ext` to the machine type.
    */
  def supportsExtendedMemory: Boolean

  /**
    * Whether this family only supports Hyperdisk block storage (no Persistent Disk, no Local SSD).
    */
  def requiresHyperdisk: Boolean = false

  // Shown in metadata for the `machineFamily` runtime attribute
  override def toString: String = family

  /**
    * The minimum memory per cpu.
    */
  def minMemoryPerCpu: MemorySize

  /**
    * The maximum memory per cpu.
    */
  def maxMemoryPerCpu: MemorySize

  /**
    * The total memory for a custom machine type must be a multiple of this value.
    */
  def memoryFactor: MemorySize

  /**
    * Increase the cpu to the next valid amount for this machine type.
    */
  def validateCpu(cpu: Int Refined Positive): Int

  /**
    * Increase the memory to the next valid amount for this machine type.
    */
  def validateMemory(memory: MemorySize): MemorySize

  /**
    * Generates a custom machine type based on the requested memory and cpu.
    *
    * @param useExtendedMemory If true and the family supports extended memory, a memory-per-cpu ratio above
    *                          `maxMemoryPerCpu` is satisfied with extended memory instead of adding cpus.
    */
  def machineType(requestedMemory: MemorySize,
                  requestedCpu: Int Refined Positive,
                  jobLogger: Logger,
                  useExtendedMemory: Boolean = false
  ): String = {
    val memory = requestedMemory |> validateMemory
    val cpu = requestedCpu |> validateCpu

    val memoryPerCpuRatio = memory.bytes / cpu.toDouble

    lazy val adjustedMemory = MemorySize(minMemoryPerCpu.amount * cpu.toDouble, minMemoryPerCpu.unit) |> validateMemory

    lazy val adjustedCpu = refineV[Positive]((memory.bytes / maxMemoryPerCpu.bytes).ceil.toInt) match {
      // If for some reason the above yields 0, keep the cpu value unchanged
      case Left(_) => cpu
      case Right(adjusted) => adjusted |> validateCpu
    }

    val extendedMemory = useExtendedMemory && supportsExtendedMemory && memoryPerCpuRatio > maxMemoryPerCpu.bytes

    val (validCpu, validMemory) =
      if (extendedMemory) {
        // Keep the requested cpu and bill the memory above the max ratio as extended memory
        (cpu, memory)
      } else if (memoryPerCpuRatio < minMemoryPerCpu.bytes) {
        // If we're under the ratio, top up the memory. Because validMemory will only increase memory (if needed),
        // there's no risk that the call to validMemory will make the ratio invalid
        (cpu, adjustedMemory)
      } else if (memoryPerCpuRatio > maxMemoryPerCpu.bytes) {
        // If we're over the ratio, top up the CPU. Because validCpu will only increase CPU (if needed), there's no risk
        // that the call to validCpu will make the ratio invalid
        (adjustedCpu, memory)
      } else {
        //
        (cpu, memory)
      }
    logAdjustment(requestedCpu.value, validCpu, requestedMemory, validMemory, jobLogger)
    val extSuffix = if (extendedMemory) "-ext" else ""
    s"${vmTypePrefix}custom-$validCpu-${validMemory.to(MemoryUnit.MB).amount.intValue()}$extSuffix"
  }

  private def logAdjustment(originalCpu: Int,
                            adjustedCpu: Int,
                            originalMemory: MemorySize,
                            adjustedMemory: MemorySize,
                            logger: Logger
  ): Unit = {
    def memoryAdjustmentLog = s"memory was adjusted from ${originalMemory.toMBString} to ${adjustedMemory.toMBString}"

    def cpuAdjustmentLog = s"cpu was adjusted from $originalCpu to $adjustedCpu"

    val messageOption =
      (
        originalCpu == adjustedCpu,
        originalMemory.to(MemoryUnit.MB).amount == adjustedMemory.to(MemoryUnit.MB).amount
      ) match {
        case (true, false) => Option(memoryAdjustmentLog)
        case (false, true) => Option(cpuAdjustmentLog)
        case (false, false) => Option(memoryAdjustmentLog + " and " + cpuAdjustmentLog)
        case _ => None
      }

    messageOption foreach { message => logger.info("To comply with GCE custom machine requirements, " + message) }
  }
}

object CustomMachineType {

  /**
    * Machine families selectable with the `machineFamily` runtime attribute.
    */
  lazy val supportedFamilies: List[CustomMachineType] = List(
    N1CustomMachineType,
    N2CustomMachineType,
    N2DCustomMachineType,
    N4CustomMachineType,
    N4DCustomMachineType,
    E2CustomMachineType
  )

  def fromFamily(family: String): Option[CustomMachineType] =
    supportedFamilies.find(_.family.equalsIgnoreCase(family.trim))

  /**
    * Valid cpu counts for N2D and N4D: 2, 4, 8, 16, then multiples of 16 up to 96.
    */
  private[models] def validateAmdCpu(cpu: Int): Int =
    cpu match {
      case cpu if cpu <= 16 => 2 max pow(2, (log(cpu.toDouble) / log(2)).ceil).toInt
      case cpu if cpu > 16 && cpu <= 96 && cpu % 16 == 0 => cpu
      case cpu if cpu > 16 && cpu <= 96 => cpu + 16 - (cpu % 16)
      case _ => 96
    }

  implicit class EnhancedInformation(val information: MemorySize) extends AnyVal {
    def asMultipleOf(factor: MemorySize): MemorySize =
      MemorySize(factor.amount * (information.bytes / factor.bytes).ceil, factor.unit)

    def toMBString: String = information.to(MemoryUnit.MB).toString
  }
}

case object N1CustomMachineType extends CustomMachineType {
  override val family: String = "n1"
  // For now using the legacy empty prefix that implies "n1-"
  override val vmTypePrefix: String = ""
  override val supportsExtendedMemory: Boolean = true
  override val minMemoryPerCpu: MemorySize = MemorySize(0.9, MemoryUnit.GB)
  override val maxMemoryPerCpu: MemorySize = MemorySize(6.5, MemoryUnit.GB)
  override val memoryFactor: MemorySize = MemorySize(256, MemoryUnit.MB)

  override def validateCpu(cpu: Refined[Int, Positive]): Int =
    // Either one cpu, or an even number of cpus
    cpu.value match {
      case 1 => 1
      case cpu => cpu + (cpu % 2)
    }

  override def validateMemory(memory: MemorySize): MemorySize =
    memory.asMultipleOf(memoryFactor)
}

case object N2CustomMachineType extends CustomMachineType {
  override val family: String = "n2"
  override val vmTypePrefix: String = "n2-"
  override val supportsExtendedMemory: Boolean = true
  override val minMemoryPerCpu: MemorySize = MemorySize(1.0, MemoryUnit.GB)
  override val maxMemoryPerCpu: MemorySize = MemorySize(8.0, MemoryUnit.GB)
  override val memoryFactor: MemorySize = MemorySize(256, MemoryUnit.MB)

  override def validateCpu(cpu: Refined[Int, Positive]): Int =
    // cpus must be divisible by 2 up to 32, and higher numbers must be divisible by 4
    cpu.value match {
      case cpu if cpu <= 32 => cpu + (cpu % 2)
      case cpu if cpu % 4 == 0 => cpu
      case cpu => cpu + (4 - (cpu % 4))
    }

  override def validateMemory(memory: MemorySize): MemorySize =
    memory.asMultipleOf(memoryFactor)
}

case object N2DCustomMachineType extends CustomMachineType {
  override val family: String = "n2d"
  override val vmTypePrefix: String = "n2d-"
  override val supportsExtendedMemory: Boolean = true
  override val minMemoryPerCpu: MemorySize = MemorySize(0.5, MemoryUnit.GB)
  override val maxMemoryPerCpu: MemorySize = MemorySize(8.0, MemoryUnit.GB)
  override val memoryFactor: MemorySize = MemorySize(256, MemoryUnit.MB)

  override def validateCpu(cpu: Refined[Int, Positive]): Int = CustomMachineType.validateAmdCpu(cpu.value)

  override def validateMemory(memory: MemorySize): MemorySize =
    memory.asMultipleOf(memoryFactor)
}

/**
  * N4 machines only support Hyperdisk storage.
  */
case object N4CustomMachineType extends CustomMachineType {
  override val family: String = "n4"
  override val vmTypePrefix: String = "n4-"
  override val supportsExtendedMemory: Boolean = true
  override val requiresHyperdisk: Boolean = true
  override val minMemoryPerCpu: MemorySize = MemorySize(2.0, MemoryUnit.GB)
  override val maxMemoryPerCpu: MemorySize = MemorySize(8.0, MemoryUnit.GB)
  override val memoryFactor: MemorySize = MemorySize(256, MemoryUnit.MB)

  override def validateCpu(cpu: Refined[Int, Positive]): Int =
    // Any multiple of 2, between 2 and 80
    (2 max (cpu.value + (cpu.value % 2))) min 80

  override def validateMemory(memory: MemorySize): MemorySize =
    memory.asMultipleOf(memoryFactor)
}

/**
  * N4D machines only support Hyperdisk storage.
  */
case object N4DCustomMachineType extends CustomMachineType {
  override val family: String = "n4d"
  override val vmTypePrefix: String = "n4d-"
  override val supportsExtendedMemory: Boolean = true
  override val requiresHyperdisk: Boolean = true
  override val minMemoryPerCpu: MemorySize = MemorySize(0.5, MemoryUnit.GB)
  override val maxMemoryPerCpu: MemorySize = MemorySize(8.0, MemoryUnit.GB)
  override val memoryFactor: MemorySize = MemorySize(256, MemoryUnit.MB)

  override def validateCpu(cpu: Refined[Int, Positive]): Int = CustomMachineType.validateAmdCpu(cpu.value)

  override def validateMemory(memory: MemorySize): MemorySize =
    memory.asMultipleOf(memoryFactor)
}

/**
  * E2 machines do not support extended memory, so memory above 8 GB per cpu always adds cpus.
  */
case object E2CustomMachineType extends CustomMachineType {
  override val family: String = "e2"
  override val vmTypePrefix: String = "e2-"
  override val supportsExtendedMemory: Boolean = false
  override val minMemoryPerCpu: MemorySize = MemorySize(0.5, MemoryUnit.GB)
  override val maxMemoryPerCpu: MemorySize = MemorySize(8.0, MemoryUnit.GB)
  override val memoryFactor: MemorySize = MemorySize(256, MemoryUnit.MB)

  val maxCpu: Int = 32
  val maxMemory: MemorySize = MemorySize(128, MemoryUnit.GB)

  override def validateCpu(cpu: Refined[Int, Positive]): Int =
    // Any multiple of 2, between 2 and 32
    (2 max (cpu.value + (cpu.value % 2))) min maxCpu

  override def validateMemory(memory: MemorySize): MemorySize =
    memory.asMultipleOf(memoryFactor)
}
