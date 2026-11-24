package cromwell.backend.google.batch.util

import cats.implicits.catsSyntaxValidatedId
import com.typesafe.config.Config
import common.validation.ErrorOr.ErrorOr
import cromwell.backend.google.batch.models.{CustomMachineType, GcpBatchRuntimeAttributes}
import cromwell.backend.validation.{OptionalRuntimeAttributesValidation, RuntimeAttributesValidation}
import wom.types.{WomStringType, WomType}
import wom.values.{WomString, WomValue}

/**
  * Validates the `machineFamily` runtime attribute, e.g. "n2d", into the matching custom machine type family.
  */
object MachineFamilyValidation {
  lazy val instance: RuntimeAttributesValidation[CustomMachineType] = new MachineFamilyValidation
  lazy val optional: OptionalRuntimeAttributesValidation[CustomMachineType] = instance.optional

  /**
    * Optional validation that falls back to a `machineFamily` set in the backend's `default-runtime-attributes`.
    */
  def optionalWithConfigDefault(runtimeConfig: Option[Config]): OptionalRuntimeAttributesValidation[CustomMachineType] =
    instance.configDefaultWomValue(runtimeConfig) match {
      case Some(default) => instance.withDefault(default).optional
      case None => optional
    }

  val supportedFamiliesString: String = CustomMachineType.supportedFamilies.map(_.family).mkString(", ")
}

class MachineFamilyValidation extends RuntimeAttributesValidation[CustomMachineType] {
  override def key: String = GcpBatchRuntimeAttributes.MachineFamilyKey

  override def coercion: Iterable[WomType] = Set(WomStringType)

  override def validateValue: PartialFunction[WomValue, ErrorOr[CustomMachineType]] = {
    case WomString(s) =>
      CustomMachineType.fromFamily(s) match {
        case Some(family) => family.validNel
        case None =>
          s"Invalid '$key': '$s' is not a supported machine family. Supported families: ${MachineFamilyValidation.supportedFamiliesString}.".invalidNel
      }
    case other =>
      s"Invalid '$key': String value required but got ${other.womType.friendlyName}.".invalidNel
  }

  override protected def missingValueMessage: String =
    s"Expecting $key runtime attribute to be one of: ${MachineFamilyValidation.supportedFamiliesString}"
}
