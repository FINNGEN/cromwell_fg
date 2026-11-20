package cromwell.backend.google.batch.util

import cromwell.backend.google.batch.models.{
  CustomMachineType,
  GcpBatchRuntimeAttributes,
  N1CustomMachineType,
  N2CustomMachineType,
  N2DCustomMachineType
}
import cromwell.core.logging.JobLogger
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import wom.format.MemorySize

object GcpBatchMachineConstraints {

  /**
    * Selects the custom machine type family. An explicit `machineFamily` wins; otherwise the family is derived from
    * the cpu platform, defaulting to n1.
    */
  def customMachineType(cpuPlatformOption: Option[String],
                        machineFamilyOption: Option[CustomMachineType]
  ): CustomMachineType =
    machineFamilyOption.getOrElse {
      // If someone requests Intel Cascade Lake or Intel Ice Lake as their CPU platform then switch the machine type to n2.
      // Similarly, CPU platform of AMD Rome corresponds to the machine type n2d.
      cpuPlatformOption match {
        case Some(GcpBatchRuntimeAttributes.CpuPlatformIntelCascadeLakeValue) => N2CustomMachineType
        case Some(GcpBatchRuntimeAttributes.CpuPlatformIntelIceLakeValue) => N2CustomMachineType
        case Some(GcpBatchRuntimeAttributes.CpuPlatformAMDRomeValue) => N2DCustomMachineType
        case _ => N1CustomMachineType
      }
    }

  def machineType(memory: MemorySize,
                  cpu: Int Refined Positive,
                  cpuPlatformOption: Option[String],
                  jobLogger: JobLogger,
                  machineFamilyOption: Option[CustomMachineType] = None
  ): String =
    // When the user explicitly picks a machine family, honor the requested cpu count and satisfy high memory
    // requests with extended memory (where the family supports it) instead of adding cpus.
    customMachineType(cpuPlatformOption, machineFamilyOption).machineType(
      memory,
      cpu,
      jobLogger,
      useExtendedMemory = machineFamilyOption.isDefined
    )
}
