package xiangshan.cache.dcache

import chisel3._
import chisel3.util._
import chisel3.util.random.LFSR
import freechips.rocketchip.util._
import freechips.rocketchip.util.property.cover
import org.chipsalliance.cde.config.Parameters
import xiangshan.cache.DCacheModule
import xiangshan.cache.DCacheBundle
import xiangshan.CSROpType.seti
import freechips.rocketchip.regmapper.RRTest0Map.de

// class setBIP(n_sets: Int, n_ways: Int) {
//   def nBits = n_ways - 1
//   protected val state_vec = if (nBits == 0) Reg(Vec(n_sets, UInt(0.W))) else RegInit(Vec(n_sets, 0.U(nBits.W)))
//   protected val bip_cnt = RegInit(0.U(bipcnt_bits.W))
//   protected val repl_plru = new PseudoLRU(n_ways)

//   def access(valid: Bool, set: UInt, touch_way: UInt, req_info: UInt) = {
//     require(req_info.getWidth == 1)
//     val hit = req_info(0)
//     val miss = !req_info(0)
//     when (valid) {
//       when (hit || miss && bip_cnt === 0.U) {
//         state_vec(set) := repl_plru.get_next_state(state_vec(set), touch_way)
//       }
//     }
//   }

//   def access(valids: Seq[Bool], sets: Seq[UInt], touch_ways: Seq[UInt], req_infos: Seq[UInt]) = {
//     require(Seq(sets.size, touch_ways.size, req_infos.size).foldLeft(true)((acc, size) => acc && (size == valids.size)), "size should be same")
//     require(req_infos.map(_.getWidth == 1).reduce(_ && _))
//     assert(PopCount((valids zip req_infos).map { case (valid, info) => valid && !info(0)}) <= 1.U, "Must only one replace in precess")
//     for (set <- 0 until n_sets) {
//       when ((valids zip sets).map{case (v,s) => v && (s === set.U)}.reduce(_ && _)) {
//         state_vec(set) := (valids zip sets zip touch_ways zip req_infos).foldLeft(state_vec(set)) { 
//           case (state, (((valid, touch_set), touch_way), req_info)) =>
//             val hit = req_info(0)
//             val miss = !req_info(0)
//             val next = Mux(valid && touch_set === set.U && (hit || miss && bip_cnt === 0.U),
//               repl_plru.get_next_state(state, touch_way), state)
//             next
//     }}}
//   }

//   def way(set: UInt, valid: Bool): UInt = {
//     when (valid) {
//       bip_cnt := bip_cnt + 1.U
//     }
//     repl_plru.get_replace_way(state_vec(set))
//   }
// }

// class DynamicInsertPolicy(dedicated_sets_num: Int, psel_bits: Int) {
//   def match_a(set: UInt) = {
//     val set_bits = set.getWidth
//     val dedicated_set_bits = log2Ceil(dedicated_sets_num)
//     require((set_bits - dedicated_set_bits) > (dedicated_set_bits - 1))
//     set(set_bits - 1, set_bits - dedicated_set_bits) === set(dedicated_set_bits - 1, 0)
//   }
//   def match_b(set: UInt) = {
//     val set_bits = set.getWidth
//     val dedicated_set_bits = log2Ceil(dedicated_sets_num)
//     require((set_bits - dedicated_set_bits) > (dedicated_set_bits - 1))
//     set(set_bits - 1, set_bits - dedicated_set_bits) === ~(set(dedicated_set_bits - 1, 0)).asUInt
//   }
// }

class LduAccess(implicit p: Parameters) extends DCacheBundle {
  val set = UInt(idxBits.W)
  val way = UInt(wayBits.W)
  val hit = Bool()
  def isHit = hit
  def isMiss = !hit
}

class MPAccess(implicit p: Parameters) extends LduAccess {
  val repl = Bool() // if repl is true, ignore hit
  override def isHit = !repl && hit
  override def isMiss = !repl && !hit
}

class ReplReq(implicit p: Parameters) extends DCacheBundle {
  val set = UInt(idxBits.W)
}

class ReplResp(implicit p: Parameters) extends DCacheBundle {
  val way = UInt(wayBits.W)
}

class DIP(implicit p: Parameters) extends DCacheModule {
  val n_ways = cacheParams.nWays
  val n_sets = cacheParams.nSets
  val dedicated_sets_num = 16
  val state_bits = n_ways - 1
  val psel_bits = 10
  val bipcnt_bits = 5
  val pselInit = (1 << (psel_bits - 1)).U(psel_bits.W)
  val pselMax = ~(0.U(psel_bits.W))

  val in = IO(Input(new Bundle {
    val lduAccess = Vec(LoadPipelineWidth, ValidIO(new LduAccess))
    val mpAccess = ValidIO(new MPAccess)
    val replReq = ValidIO(new ReplReq)
  }))

  val out = IO(Output(new Bundle{
    val replResp = new ReplResp
  }))

  val state_vec = if (state_bits == 0) Reg(Vec(n_sets, UInt(0.W))) else RegInit(VecInit(Seq.fill(n_sets)(0.U(state_bits.W))))
  val bip_cnt = RegInit(0.U(bipcnt_bits.W))
  val repl_plru = new PseudoLRU(n_ways)
  val psel = RegInit(pselInit)

  def match_a(set: UInt) = {
    val set_bits = set.getWidth
    val dedicated_set_bits = log2Ceil(dedicated_sets_num)
    require((set_bits - dedicated_set_bits) > (dedicated_set_bits - 1), s"$set_bits, $dedicated_set_bits")
    set(set_bits - 1, set_bits - dedicated_set_bits) === set(dedicated_set_bits - 1, 0)
  }

  def match_b(set: UInt) = {
    val set_bits = set.getWidth
    val dedicated_set_bits = log2Ceil(dedicated_sets_num)
    require((set_bits - dedicated_set_bits) > (dedicated_set_bits - 1))
    set(set_bits - 1, set_bits - dedicated_set_bits) === ~(set(dedicated_set_bits - 1, 0))
  }

  def follower(set: UInt) = {
    !match_a(set) && !match_b(set)
  }

  def use_a = !psel(psel_bits - 1)
  def use_b = psel(psel_bits - 1)

  // hit: insert mru
  // miss: update psel
  // repl: insert lru or mru
  state_vec.zipWithIndex.foreach {
    case (state, setidx) =>
      val set = setidx.U(idxBits.W)
      val updateState = in.lduAccess.map(a => a.valid && a.bits.set === set).asUInt.orR || in.mpAccess.valid
      val touchWaysSeq = in.lduAccess.map {a =>
        val touch_ways = Wire(Valid(UInt(wayBits.W)))
        touch_ways.valid := a.valid && a.bits.hit && a.bits.set === set
        touch_ways.bits := a.bits.way
        touch_ways
      } ++ Seq {
        val mpAccess = in.mpAccess.bits
        val touch_way = Wire(Valid(UInt(wayBits.W)))
        touch_way.valid := in.mpAccess.valid && in.mpAccess.bits.set === set && Mux(mpAccess.repl,
          match_a(set) || match_b(set) && bip_cnt === 0.U || follower(set) && (use_a || use_b && bip_cnt === 0.U),
          mpAccess.hit)
        touch_way.bits := in.mpAccess.bits.way
        touch_way
      }
      when (updateState) {
        state := repl_plru.get_next_state(state, touchWaysSeq)
      }
  }

  val accesses = in.lduAccess ++ Seq(in.mpAccess)
  val aMiss = PopCount(VecInit(accesses.map {
    case a => a.valid && match_a(a.bits.set) && a.bits.isMiss
  }))

  val bMiss = PopCount(VecInit(accesses.map {
    case a => a.valid && match_b(a.bits.set) && a.bits.isMiss
  }))

  val pselUpd = psel + aMiss - bMiss

  // another way is only add aMiss when too small or only sub bMiss when too large
  when (accesses.map(_.valid).asUInt.orR) {
    psel := MuxCase(pselUpd, Seq(
      (pselUpd < pselInit && psel > pselInit) -> pselMax,
      (pselUpd > pselInit && psel < pselInit) -> 0.U
    ))
  }

  out.replResp.way := repl_plru.get_replace_way(state_vec(in.replReq.bits.set))
  val mpAccessSet = in.mpAccess.bits.set
  when (in.mpAccess.valid && in.mpAccess.bits.repl && (match_b(mpAccessSet) || follower(mpAccessSet) && use_b)) {
    bip_cnt := bip_cnt + 1.U
  }
}
