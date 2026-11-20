package cromwell.backend.google.batch.util

import common.assertion.CromwellTimeoutSpec
import common.mock.MockSugar.mock
import cromwell.backend.google.batch.models.{
  CustomMachineType,
  E2CustomMachineType,
  GcpBatchRuntimeAttributes,
  N1CustomMachineType,
  N2CustomMachineType,
  N2DCustomMachineType,
  N4CustomMachineType,
  N4DCustomMachineType
}
import cromwell.core.logging.JobLogger
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineMV
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.Tables.Table
import wdl4s.parser.MemoryUnit
import wom.format.MemorySize

class GcpBatchMachineConstraintsSpec extends AnyFlatSpec with CromwellTimeoutSpec with Matchers {
  behavior of "MachineConstraints"

  private val n2OptionCascadeLake = Option(GcpBatchRuntimeAttributes.CpuPlatformIntelCascadeLakeValue)

  private val n2dOption = Option(GcpBatchRuntimeAttributes.CpuPlatformAMDRomeValue)

  private val n2OptionIceLake = Option(GcpBatchRuntimeAttributes.CpuPlatformIntelIceLakeValue)

  it should "generate valid machine types" in {
    val validTypes = Table(
      ("memory", "cpu", "cpuPlatformOption", "machineTypeString"),
      // Already ok tuple
      (MemorySize(1024, MemoryUnit.MB), refineMV[Positive](1), None, "custom-1-1024"),
      // CPU must be even (except if it's 1)
      (MemorySize(4, MemoryUnit.GB), refineMV[Positive](3), None, "custom-4-4096"),
      // Memory must be a multiple of 256
      (MemorySize(1, MemoryUnit.GB), refineMV[Positive](1), None, "custom-1-1024"),
      // Memory / cpu ratio must be > 0.9GB, increase memory
      (MemorySize(1, MemoryUnit.GB), refineMV[Positive](4), None, "custom-4-3840"),
      (MemorySize(14, MemoryUnit.GB), refineMV[Positive](16), None, "custom-16-14848"),
      // Memory / cpu ratio must be < 6.5GB, increase CPU
      (MemorySize(13.65, MemoryUnit.GB), refineMV[Positive](1), None, "custom-4-14080"),
      // Memory should be an int
      (MemorySize(1520.96, MemoryUnit.MB), refineMV[Positive](1), None, "custom-1-1536"),
      (MemorySize(1024.0, MemoryUnit.MB), refineMV[Positive](1), None, "custom-1-1024"),
      // Increase to a cpu selection not valid for n2 below
      (MemorySize(2, MemoryUnit.GB), refineMV[Positive](33), None, "custom-34-31488"),

      // Same tests but with cascade lake (n2)
      (MemorySize(1024, MemoryUnit.MB), refineMV[Positive](1), n2OptionCascadeLake, "n2-custom-2-2048"),
      (MemorySize(4, MemoryUnit.GB), refineMV[Positive](3), n2OptionCascadeLake, "n2-custom-4-4096"),
      (MemorySize(1, MemoryUnit.GB), refineMV[Positive](1), n2OptionCascadeLake, "n2-custom-2-2048"),
      (MemorySize(1, MemoryUnit.GB), refineMV[Positive](4), n2OptionCascadeLake, "n2-custom-4-4096"),
      (MemorySize(14, MemoryUnit.GB), refineMV[Positive](16), n2OptionCascadeLake, "n2-custom-16-16384"),
      (MemorySize(13.65, MemoryUnit.GB), refineMV[Positive](1), n2OptionCascadeLake, "n2-custom-2-14080"),
      (MemorySize(1520.96, MemoryUnit.MB), refineMV[Positive](1), n2OptionCascadeLake, "n2-custom-2-2048"),
      (MemorySize(1024.0, MemoryUnit.MB), refineMV[Positive](1), n2OptionCascadeLake, "n2-custom-2-2048"),
      (MemorySize(2, MemoryUnit.GB), refineMV[Positive](33), n2OptionCascadeLake, "n2-custom-36-36864"),

      // Same tests, but with ice lake. Should produce same results as cascade lake since they're both n2.
      (MemorySize(1024, MemoryUnit.MB), refineMV[Positive](1), n2OptionIceLake, "n2-custom-2-2048"),
      (MemorySize(4, MemoryUnit.GB), refineMV[Positive](3), n2OptionIceLake, "n2-custom-4-4096"),
      (MemorySize(1, MemoryUnit.GB), refineMV[Positive](1), n2OptionIceLake, "n2-custom-2-2048"),
      (MemorySize(1, MemoryUnit.GB), refineMV[Positive](4), n2OptionIceLake, "n2-custom-4-4096"),
      (MemorySize(14, MemoryUnit.GB), refineMV[Positive](16), n2OptionIceLake, "n2-custom-16-16384"),
      (MemorySize(13.65, MemoryUnit.GB), refineMV[Positive](1), n2OptionIceLake, "n2-custom-2-14080"),
      (MemorySize(1520.96, MemoryUnit.MB), refineMV[Positive](1), n2OptionIceLake, "n2-custom-2-2048"),
      (MemorySize(1024.0, MemoryUnit.MB), refineMV[Positive](1), n2OptionIceLake, "n2-custom-2-2048"),
      (MemorySize(2, MemoryUnit.GB), refineMV[Positive](33), n2OptionIceLake, "n2-custom-36-36864"),

      // Same tests but with AMD Rome (n2d) #cpu > 16 are in increments of 16
      (MemorySize(1024, MemoryUnit.MB), refineMV[Positive](1), n2dOption, "n2d-custom-2-1024"),
      (MemorySize(4, MemoryUnit.GB), refineMV[Positive](3), n2dOption, "n2d-custom-4-4096"),
      (MemorySize(1, MemoryUnit.GB), refineMV[Positive](1), n2dOption, "n2d-custom-2-1024"),
      (MemorySize(1, MemoryUnit.GB), refineMV[Positive](4), n2dOption, "n2d-custom-4-2048"),
      (MemorySize(14, MemoryUnit.GB), refineMV[Positive](16), n2dOption, "n2d-custom-16-14336"),
      (MemorySize(13.65, MemoryUnit.GB), refineMV[Positive](1), n2dOption, "n2d-custom-2-14080"),
      (MemorySize(1520.96, MemoryUnit.MB), refineMV[Positive](1), n2dOption, "n2d-custom-2-1536"),
      (MemorySize(1024.0, MemoryUnit.MB), refineMV[Positive](1), n2dOption, "n2d-custom-2-1024"),
      (MemorySize(2, MemoryUnit.GB), refineMV[Positive](33), n2dOption, "n2d-custom-48-24576"),
      (MemorySize(2, MemoryUnit.GB), refineMV[Positive](81), n2dOption, "n2d-custom-96-49152"),
      (MemorySize(256, MemoryUnit.GB), refineMV[Positive](128), n2dOption, "n2d-custom-96-262144")
    )

    forAll(validTypes) { (memory, cpu, cpuPlatformOption, expected) =>
      GcpBatchMachineConstraints.machineType(
        memory = memory,
        cpu = cpu,
        cpuPlatformOption = cpuPlatformOption,
        jobLogger = mock[JobLogger]
      ) shouldBe expected
    }
  }

  it should "generate valid machine types for an explicit machine family" in {
    val validTypes = Table(
      ("memory", "cpu", "cpuPlatformOption", "machineFamily", "machineTypeString"),
      // n4: even cpus between 2 and 80, 2-8 GB per cpu
      (MemorySize(1, MemoryUnit.GB), refineMV[Positive](1), None, N4CustomMachineType, "n4-custom-2-4096"),
      (MemorySize(4, MemoryUnit.GB), refineMV[Positive](3), None, N4CustomMachineType, "n4-custom-4-8192"),
      (MemorySize(200, MemoryUnit.GB), refineMV[Positive](81), None, N4CustomMachineType, "n4-custom-80-204800"),
      // n4d: 2, 4, 8, 16, then multiples of 16 up to 96, 0.5-8 GB per cpu
      (MemorySize(4, MemoryUnit.GB), refineMV[Positive](3), None, N4DCustomMachineType, "n4d-custom-4-4096"),
      (MemorySize(2, MemoryUnit.GB), refineMV[Positive](33), None, N4DCustomMachineType, "n4d-custom-48-24576"),
      // e2: even cpus between 2 and 32, 0.5-8 GB per cpu, no extended memory so cpus are added instead
      (MemorySize(1, MemoryUnit.GB), refineMV[Positive](1), None, E2CustomMachineType, "e2-custom-2-1024"),
      (MemorySize(32, MemoryUnit.GB), refineMV[Positive](2), None, E2CustomMachineType, "e2-custom-4-32768"),
      // n2d family wins over the cpu platform's implied n2
      (MemorySize(1, MemoryUnit.GB),
       refineMV[Positive](1),
       n2OptionCascadeLake,
       N2DCustomMachineType,
       "n2d-custom-2-1024"
      ),
      // n1 family is the same as the default, except for extended memory
      (MemorySize(4, MemoryUnit.GB), refineMV[Positive](3), None, N1CustomMachineType, "custom-4-4096"),
      // Memory above the max ratio uses extended memory instead of adding cpus
      (MemorySize(13.65, MemoryUnit.GB), refineMV[Positive](1), None, N1CustomMachineType, "custom-1-14080-ext"),
      (MemorySize(32, MemoryUnit.GB), refineMV[Positive](2), None, N2CustomMachineType, "n2-custom-2-32768-ext"),
      (MemorySize(64, MemoryUnit.GB), refineMV[Positive](2), None, N2DCustomMachineType, "n2d-custom-2-65536-ext"),
      (MemorySize(32, MemoryUnit.GB), refineMV[Positive](2), None, N4CustomMachineType, "n4-custom-2-32768-ext"),
      (MemorySize(64, MemoryUnit.GB), refineMV[Positive](2), None, N4DCustomMachineType, "n4d-custom-2-65536-ext")
    )

    forAll(validTypes) { (memory, cpu, cpuPlatformOption, machineFamily, expected) =>
      GcpBatchMachineConstraints.machineType(
        memory = memory,
        cpu = cpu,
        cpuPlatformOption = cpuPlatformOption,
        jobLogger = mock[JobLogger],
        machineFamilyOption = Option(machineFamily)
      ) shouldBe expected
    }
  }

  it should "look up machine families case-insensitively" in {
    CustomMachineType.fromFamily("N4") shouldBe Option(N4CustomMachineType)
    CustomMachineType.fromFamily(" n2d ") shouldBe Option(N2DCustomMachineType)
    CustomMachineType.fromFamily("c3") shouldBe None
  }
}
