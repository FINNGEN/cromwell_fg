package cromwell.backend.google.batch.util

import common.assertion.CromwellTimeoutSpec
import cromwell.backend.google.batch.io.{BatchApiEmptyMountedDisk, DiskType, GcpBatchWorkingDisk}
import cromwell.core.path.DefaultPathBuilder
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BatchUtilityConversionsSpec extends AnyFlatSpec with CromwellTimeoutSpec with Matchers {
  behavior of "BatchUtilityConversions"

  private val conversions = new BatchUtilityConversions {}

  private val disks = List(
    GcpBatchWorkingDisk(DiskType.SSD, 10),
    BatchApiEmptyMountedDisk(DiskType.HDD, 20, DefaultPathBuilder.get("/mnt/hdd")),
    BatchApiEmptyMountedDisk(DiskType.LOCAL, 375, DefaultPathBuilder.get("/mnt/local"))
  )

  it should "map disk types to Persistent Disk types by default" in {
    conversions.toDisks(disks).map(_.getNewDisk.getType) shouldBe List("pd-ssd", "pd-standard", "local-ssd")
  }

  it should "map SSD and HDD disk types to Hyperdisk Balanced for Hyperdisk-only machine families" in {
    conversions.toDisks(disks.take(2), hyperdiskOnly = true).map(_.getNewDisk.getType) shouldBe
      List("hyperdisk-balanced", "hyperdisk-balanced")
  }

  it should "build a Hyperdisk Balanced boot disk without image or size" in {
    conversions.hyperdiskBootDisk.getType shouldBe "hyperdisk-balanced"
    conversions.hyperdiskBootDisk.getSizeGb shouldBe 0L
    conversions.hyperdiskBootDisk.getImage shouldBe ""
  }
}
