package cromwell.backend.google.batch.util

import com.google.cloud.batch.v1.AllocationPolicy.{Accelerator, AttachedDisk, Disk, ProvisioningModel}
import com.google.cloud.batch.v1.Volume
import cromwell.backend.google.batch.io.{DiskType, GcpBatchAttachedDisk, GcpBatchReferenceFilesDisk}
import cromwell.backend.google.batch.models.{GcpBatchRuntimeAttributes, GpuResource}
import wom.format.MemorySize

trait BatchUtilityConversions {

  // construct zones string
  def toZonesPath(zones: Vector[String]): Vector[String] =
    zones.map(zone => "zones/" + zone)

  // Gets first zone in vector and removes last two characters of zone to construct region
  // Ex. us-central1-a -> us-central1
  def zonesToRegion(zones: Vector[String]): Option[String] =
    zones.headOption.map(_.dropRight(2))

  // lowercase text to match gcp label requirements
  def toLabel(text: String): String =
    text.toLowerCase

  // creates batch run time location from zones entered in runtime.  This is needed if network not defined by user to place on right network.
  def toBatchRunLocation(zones: Vector[String]): String = {
    val parts = zones.mkString(",").split("-")
    parts(0) + "-" + parts(1)
  }

  // convert cpu cores to millicores that Batch expects
  def toCpuCores(cpu: Long): Long =
    cpu * 1000

  // convert memory to MiB that Batch expects
  def toMemMib(memory: MemorySize): Long =
    (memory.amount * 1024).toLong

  // set Standard or Spot instances
  def toProvisioningModel(preemptible: Boolean): ProvisioningModel =
    if (preemptible) ProvisioningModel.SPOT else ProvisioningModel.STANDARD

  /**
    * @param hyperdiskOnly If true (e.g. for N4/N4D machines), SSD and HDD disks are created as Hyperdisk Balanced.
    */
  def toDisks(disks: Seq[GcpBatchAttachedDisk], hyperdiskOnly: Boolean = false): List[AttachedDisk] =
    disks.map(toDisk(_, hyperdiskOnly)).toList

  def toVolumes(disks: Seq[GcpBatchAttachedDisk]): List[Volume] = disks.map(toVolume).toList

  def toVolume(disk: GcpBatchAttachedDisk): Volume = {
    val volume = Volume.newBuilder
      .setDeviceName(disk.name)
      .setMountPath(disk.mountPoint.pathAsString)

    disk match {
      case _: GcpBatchReferenceFilesDisk =>
        volume
          .addMountOptions("ro")
          .build
      case _ =>
        volume.build
    }
  }

  private def toDisk(disk: GcpBatchAttachedDisk, hyperdiskOnly: Boolean): AttachedDisk = {
    val googleDisk = Disk.newBuilder
      .setSizeGb(disk.sizeGb.toLong)
      .setType(toBatchDiskType(disk.diskType, hyperdiskOnly))

    disk match {
      case refDisk: GcpBatchReferenceFilesDisk =>
        googleDisk.setImage(refDisk.image).build
      case _ =>
        googleDisk.build
    }

    val googleAttachedDisk = AttachedDisk.newBuilder
      .setDeviceName(disk.name)
      .setNewDisk(googleDisk)
      .build
    googleAttachedDisk

  }

  private def toBatchDiskType(diskType: DiskType, hyperdiskOnly: Boolean) = diskType match {
    case DiskType.HDD | DiskType.SSD if hyperdiskOnly => HyperdiskBalancedType
    case DiskType.HDD => "pd-standard"
    case DiskType.SSD => "pd-ssd"
    case DiskType.LOCAL => "local-ssd"
  }

  val HyperdiskBalancedType = "hyperdisk-balanced"

  // Boot disk for machine families that only support Hyperdisk. Image and size are left unset so that Batch keeps
  // choosing its default image and sizes the disk from the task's ComputeResource.bootDiskMib, as for other families.
  lazy val hyperdiskBootDisk: Disk =
    Disk.newBuilder
      .setType(HyperdiskBalancedType)
      .build

  def convertGbToMib(runtimeAttributes: GcpBatchRuntimeAttributes): Long =
    (runtimeAttributes.bootDiskSize * 953.7).toLong

  // Create accelerators for GPUs
  def toAccelerator(gpuResource: GpuResource): Accelerator.Builder =
    Accelerator.newBuilder.setCount(gpuResource.gpuCount.value.toLong).setType(gpuResource.gpuType.toString)

}
