package cache

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import top.DefaultConfig
import xiangshan.cache.dcache._
import xiangshan._
import xiangshan.cache.DCacheParameters

import firrtl2.options.TargetDirAnnotation
import firrtl.stage.{FirrtlCircuitAnnotation, OutputFileAnnotation}
import logger.{LogLevelAnnotation, LogLevel}
import sys.env
import os.Path
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.PseudoLRU
import cc.xiangshan.openncb.util.sliceBitExtractionForUInt
import scala.util.Random

class DIPTest extends XSTester {
  behavior of "DIP ReplacementPolicy"
  override implicit val config: org.chipsalliance.cde.config.Parameters = defaultConfig.alterPartial({
    case XSCoreParamsKey => defaultConfig(XSTileKey).head.copy(
      // Example of how to change params
      dcacheParametersOpt = defaultConfig(XSTileKey).head.dcacheParametersOpt.map { dcacheParams =>
        dcacheParams.copy(nSets = 256, nWays = 4)}
  )})
  val nWays = config(XSCoreParamsKey).dcacheParametersOpt.get.nWays

  System.setProperty("CHISELTEST_VCD_ALL_SIGNALS", "true")
  it should "run" in {
    test(new DIP(true)).withAnnotations(Seq(
      WriteVcdAnnotation,
      VerilatorBackendAnnotation
    )) {
      m =>
        val psel_init = (1 << (m.psel_bits - 1))
        val psel_max = (1 << m.psel_bits) - 1
        def setinvalid = {
          m.in.lduAccess.foreach(_.valid.poke(false))
          m.in.mpAccess.valid.poke(false)
          m.debug.foreach {
            case io =>
              io.w.bip_cnt.valid.poke(false)
              io.w.psel.valid.poke(false)
              io.w.state.valid.poke(false)
          }
        }
        def way(set: Int): UInt = {
          m.in.replReq.valid.poke(true.B)
          m.in.replReq.bits.set.poke(set)
          m.out.replResp.way
        }
        def replMpAccess(set: Int, way: UInt) = {
          m.in.mpAccess.valid.poke(true)
          m.in.mpAccess.bits.set.poke(set)
          m.in.mpAccess.bits.way.poke(way)
          m.in.mpAccess.bits.repl.poke(true)
        }
        def hitMissAccess(input: Valid[LduAccess])(set: Int, way: UInt, hit: Boolean) = {
          input.valid.poke(true)
          input.bits.set.poke(set)
          input.bits.way.poke(way)
          input.bits.hit.poke(hit)
        }
        def step = {
          m.clock.step(1)
          setinvalid
        }
        def mpAccess(set: Int, way: UInt, hit: Boolean) = {
          hitMissAccess(m.in.mpAccess)(set, way, hit)
          m.in.mpAccess.bits.repl.poke(false)
        }
        def ld0Access = hitMissAccess(m.in.lduAccess(0))_
        def ld1Access = hitMissAccess(m.in.lduAccess(1))_
        def ld2Access = hitMissAccess(m.in.lduAccess(2))_

        def get_psel = m.debug.map(_.r.psel).get
        def get_bipcnt = m.debug.map(_.r.bip_cnt).get
        def get_state(set: Int) = m.debug.map {
          case a =>
            a.r.state.set.poke(set)
            a.r.state.value
        }.get
        def set_psel(psel: BigInt) = m.debug.map {
          case a =>
            a.w.psel.valid.poke(true)
            a.w.psel.bits.poke(psel)
        }
        def set_bipcnt(bipcnt: Int) = m.debug.map {
          case a =>
            a.w.bip_cnt.valid.poke(true)
            a.w.bip_cnt.bits.poke(bipcnt)
        }
        def set_state(set: Int, value: UInt) = m.debug.map {
          case a =>
            a.w.state.valid.poke(true)
            a.w.state.bits.set.poke(set)
            a.w.state.bits.value.poke(value)
        }

        setinvalid
        m.reset.poke(true.B)
        m.clock.step(1)
        m.reset.poke(false.B)
        var replway = 0.U
        val replwaySeq = Seq(0.U, 2.U, 1.U, 3.U)

        println("test plru:")
        for (i <- 0 until 4) {
          way(0).expect(replwaySeq(i))
          way(1).expect(0)
          replway = way(0).peek()
          replMpAccess(0, replway)
          step
        }
        ld0Access(0, 0.U, false)
        step
        way(0).expect(0)
        ld1Access(0, 1.U, false)
        ld2Access(0, 2.U, false)
        mpAccess(0, 3.U, false)
        step
        way(0).expect(0)
        ld0Access(0x11, 0.U, false)
        ld1Access(0x11, 1.U, true)
        ld2Access(0x11, 2.U, true)
        replMpAccess(0x11, 1.U)
        step
        way(0x11).expect(3)
        ld1Access(0x22, 0.U, true)
        mpAccess(0x22, 2.U, true)
        step
        way(0x22).expect(1)
        get_psel.expect(psel_init + 5)
        // psel = +5 now

        println("test bip")
        way(0xf0).expect(0)
        replMpAccess(0xf0, 0.U)
        step
        for (i <- 0 until 0x20) {
          way(0xf0).expect(2)
          way(1).expect(0)
          replMpAccess(0xf0, 2.U)
          step
        }
        for (i <- 0 until 0x20) {
          way(0xf0).expect(1.U)
          way(2).expect(0.U)
          replMpAccess(0xf0, 1.U)
          step
        }
        for (i <- 0 until 0x20) {
          way(0xc3).expect(0)
          way(3).expect(0)
          replMpAccess(0xc3, 0.U)
          step
        }
        for (i <- 0 until 0x20) {
          way(0xc3).expect(2)
          way(3).expect(0)
          replMpAccess(0xc3, 2.U)
          step
        }
        for (i <- 0 until 0x20) {
          way(0xf0).expect(3)
          way(4).expect(0)
          replMpAccess(0xf0, 3.U)
          step
        }
        way(0xf0).expect(0)
        ld0Access(0xe1, 1.U, true)
        ld1Access(0xe1, 3.U, true)
        ld2Access(0xe1, 2.U, true)
        mpAccess(0xe1, 0.U, true)
        step
        way(1).expect(0)
        way(0xe1).expect(3)

        ld0Access(0xd2, 0.U, false)
        ld1Access(0xd2, 0.U, false)
        ld2Access(0xd2, 0.U, false)
        mpAccess(0xd2, 0.U, false)
        step
        way(0xd2).expect(0)
        ld0Access(0xd2, 0.U, false)
        ld1Access(0xd2, 1.U, true)
        replMpAccess(0xd2, 0.U)
        step
        way(0xd2).expect(0)
        get_psel.expect(psel_init)
        get_bipcnt.expect(2)
        // psel = 0 now

        println("test psel in max case")
        set_psel(psel_max)
        step
        ld0Access(0xf0, 0.U, false)
        ld1Access(0xf0, 0.U, false)
        ld2Access(0xf0, 0.U, false)
        mpAccess(0, 0.U, false)
        step
        get_psel.expect(psel_max - 2)
        ld0Access(0, 0.U, false)
        ld1Access(0, 0.U, false)
        ld2Access(0, 0.U, false)
        mpAccess(0, 0.U, false)
        step
        get_psel.expect(psel_max)
        ld0Access(0, 0.U, false)
        ld1Access(0, 0.U, false)
        ld2Access(0, 0.U, true)
        mpAccess(0, 0.U, true)
        step
        get_psel.expect(psel_max)
        ld0Access(0xf0, 0.U, false)
        ld1Access(0xf0, 0.U, false)
        ld2Access(0xf0, 0.U, true)
        mpAccess(0xf0, 0.U, true)
        step
        get_psel.expect(psel_max - 2)

        println("test psel in zero case")
        set_psel(0)
        step
        ld0Access(0xf0, 0.U, false)
        ld1Access(0xf0, 0.U, false)
        ld2Access(0xf0, 0.U, false)
        mpAccess(0, 0.U, false)
        step
        get_psel.expect(0)
        ld0Access(0, 0.U, false)
        ld1Access(0, 0.U, false)
        ld2Access(0, 0.U, false)
        mpAccess(0, 0.U, false)
        step
        get_psel.expect(4)
        ld0Access(0, 0.U, false)
        ld1Access(0xf0, 0.U, false)
        ld2Access(0xf0, 0.U, false)
        mpAccess(0, 0.U, true)
        step
        get_psel.expect(3)
        ld0Access(0xf0, 0.U, false)
        ld1Access(0xf0, 0.U, false)
        ld2Access(0xf0, 0.U, false)
        mpAccess(0xf0, 0.U, false)
        step
        get_psel.expect(0)
        
        println("test dip in plru mode")
        set_psel(psel_init)
        step
        ld0Access(0xf0, 0.U, false)
        get_state(1).expect(0)
        step
        for (i <- 0 until 4) {
          way(1).expect(replwaySeq(i))
          replway = way(1).peek()
          replMpAccess(1, replway)
          step
        }
        get_psel.expect(psel_init - 1)
        ld0Access(1, 0.U, false)
        ld1Access(1, 0.U, true)
        ld2Access(1, 1.U, true)
        replMpAccess(1, 3.U)
        step
        way(0x1).expect(0)
        
        println("test dip in bip mode")
        ld0Access(0, 0.U, false)
        ld0Access(0, 0.U, false)
        set_bipcnt(1)
        step
        way(2).expect(0)
        for (i <- 1 until 0x20) {
          val replway = Random.between(0, 3).U
          replMpAccess(2, replway)
          step
          way(2).expect(replway)
        }
        replMpAccess(2, 0.U) // ignore
        step
        for (i <- 0 until 0x20) {
          way(3).expect(0)
          replMpAccess(3, 0.U)
          step
        }
        for (i <- 0 until 0x20) {
          way(3).expect(2)
          replMpAccess(3, 2.U)
          step
        }
        ld0Access(4, 0.U, true)
        ld1Access(4, 1.U, false)
        mpAccess(4, 2.U, true)
        step
        way(4).expect(1)
        ld0Access(4, 0.U, true)
        ld1Access(4, 1.U, false)
        replMpAccess(4, 2.U)
        step
        way(4).expect(2)
        
        println("test multi set")
        m.reset.poke(true.B)
        step
        m.reset.poke(false.B)
        ld0Access(0, 0.U, true)
        ld1Access(1, 0.U, false)
        ld2Access(2, 1.U, true)
        replMpAccess(0xf, 0.U)
        step
        way(0).expect(2)
        way(1).expect(0)
        way(2).expect(2)
        way(0xf).expect(2)
        ld0Access(0xf, 0.U, false)
        ld1Access(0, 2.U, true)
        ld2Access(2, 3.U, true)
        replMpAccess(1, 2.U)
        step
        way(0).expect(1)
        way(1).expect(2)
        way(2).expect(0)
        way(0xf).expect(2)
        ld0Access(0, 0.U, false)
        ld1Access(1, 2.U, true)
        replMpAccess(2, 0.U)
        step
        way(0).expect(1)
        way(1).expect(0)
        way(2).expect(2)
        way(0xf).expect(2)
    }
  }
}

