import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt(16.W))
    val dataRead = Input(UInt(32.W))
    val writeEnable = Output(Bool())
    val dataWrite = Output(UInt(32.W))
  })

  // ---------------------------------------------------------------
  // Tilstande (8 stk.)
  // ---------------------------------------------------------------
  val idle :: write :: readD :: readR :: readC :: readL :: readU :: done :: Nil = Enum(8)
  val stateReg = RegInit(idle)

  // ---------------------------------------------------------------
  // Registre
  // ---------------------------------------------------------------
  val addrReg = RegInit(0.U(9.W))   // aktuel pixel x + 20y (0..399)
  val xReg    = RegInit(0.U(5.W))   // x-koordinat (0..19)
  val yReg    = RegInit(0.U(5.W))   // y-koordinat (0..19)

  // Vinduet W: 41 pladser = pixels fra addr-20 til addr+20.
  // For hver plads gemmes kun om pixlen er laest (valid) og om den er hvid.
  val validReg = RegInit(VecInit(Seq.fill(41)(false.B)))
  val whiteReg = RegInit(VecInit(Seq.fill(41)(false.B)))

  // Faste pladser i vinduet for de fem pixels
  val U = 0   // ovenover   (addr - 20)
  val L = 19  // til venstre (addr - 1)
  val C = 20  // pixlen selv (addr)
  val R = 21  // til hoejre  (addr + 1)
  val D = 40  // nedenunder (addr + 20)

  // ---------------------------------------------------------------
  // Default values
  // ---------------------------------------------------------------
  io.done := false.B
  io.address := 0.U
  io.writeEnable := false.B
  io.dataWrite := 0.U

  // ---------------------------------------------------------------
  // Kombinatorisk beslutningslogik.
  // Tager et vindue (valid, white) og koordinaterne, og returnerer:
  //   white = pixlen skal skrives hvid (255)
  //   next  = naeste tilstand (write, hvis pixlen er afgjort,
  //           ellers den foerste ukendte nabo i raekkefoelgen d, r, c, l, u)
  // ---------------------------------------------------------------
  def decide(v: Seq[Bool], w: Seq[Bool], x: UInt, y: UInt): (Bool, UInt) = {
    val five       = Seq(U, L, C, R, D)
    val border     = x === 0.U || x === 19.U || y === 0.U || y === 19.U
    val knownBlack = five.map(i => v(i) && !w(i)).reduce(_ || _)
    val allWhite   = five.map(i => v(i) && w(i)).reduce(_ && _)
    val decided    = border || knownBlack || allWhite
    val white      = allWhite && !border

    val nextRead = PriorityMux(Seq(
      !v(D) -> readD,
      !v(R) -> readR,
      !v(C) -> readC,
      !v(L) -> readL,
      !v(U) -> readU
    ))
    (white, Mux(decided, write, nextRead))
  }

  // Laeser en nabo ind paa plads i i vinduet og vaelger naeste tilstand
  // ud fra vinduet inklusive den nye vaerdi.
  def readNeighbour(i: Int, addr: UInt): Unit = {
    io.address := addr
    val isWhite = io.dataRead =/= 0.U
    validReg(i) := true.B
    whiteReg(i) := isWhite

    val vNew = (0 until 41).map(j => if (j == i) true.B else validReg(j))
    val wNew = (0 until 41).map(j => if (j == i) isWhite else whiteReg(j))
    val (_, nxt) = decide(vNew, wNew, xReg, yReg)
    stateReg := nxt
  }

  // ---------------------------------------------------------------
  // Tilstandsmaskinen
  // ---------------------------------------------------------------
  switch(stateReg) {

    is(idle) {
      when(io.start) {
        addrReg := 0.U
        xReg := 0.U
        yReg := 0.U
        for (i <- 0 until 41) { validReg(i) := false.B }
        stateReg := write
      }
    }

    is(write) {
      val (white, _) = decide(validReg, whiteReg, xReg, yReg)
      io.address := addrReg +& 400.U   // +& saa resultatet kan blive op til 799
      io.writeEnable := true.B
      io.dataWrite := Mux(white, 255.U, 0.U)

      when(addrReg === 399.U) {
        stateReg := done
      }.otherwise {
        // advance: naeste pixel
        val nextX = Mux(xReg === 19.U, 0.U, xReg + 1.U)
        val nextY = Mux(xReg === 19.U, yReg + 1.U, yReg)
        addrReg := addrReg + 1.U
        xReg := nextX
        yReg := nextY

        // Forskyd vinduet en plads. Den nye plads W[40] er ukendt.
        val vShift = (1 until 41).map(i => validReg(i)) :+ false.B
        val wShift = (1 until 41).map(i => whiteReg(i)) :+ false.B
        for (i <- 0 until 41) {
          validReg(i) := vShift(i)
          whiteReg(i) := wShift(i)
        }

        // Naeste tilstand afgoeres for den naeste pixel (decided')
        val (_, nxt) = decide(vShift, wShift, nextX, nextY)
        stateReg := nxt
      }
    }

    is(readD) { readNeighbour(D, addrReg + 20.U) }
    is(readR) { readNeighbour(R, addrReg + 1.U) }
    is(readC) { readNeighbour(C, addrReg) }
    is(readL) { readNeighbour(L, addrReg - 1.U) }
    is(readU) { readNeighbour(U, addrReg - 20.U) }

    is(done) {
      io.done := true.B
    }
  }
}