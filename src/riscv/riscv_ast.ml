(* Definitions for RISC-V *)
(* CS443 - Stefan Muller *)

type reg = X0 (* Constant zero *)
         | X1 (* Return address (callee-saved) *)
         | X2 (* Stack pointer (callee-saved) *)
         | X3 | X4 (* Reserved *)
         | X5 | X6 | X7 (* We'll use these as temporaries for loads/stores *)
         | X8 (* Frame pointer (callee-saved) *)
         | X9 (* Callee-saved *)
         | X10 | X11 (* Arguments/return values (caller-saved) *)
         | X12 | X13 | X14 | X15 | X16 | X17 (* Arguments (caller-saved) *)
         (* Callee-saved temporaries *)
         | X18 | X19 | X20 | X21 | X22 | X23 | X24 | X25 | X26 | X27
         | X28 | X29 | X30 | X31 (* Caller-saved temporaries *)

let callee_saved = [X9; X18; X19; X20; X21; X22; X23; X24; X25; X26; X27]

let caller_saved = [X5; X6; X7; X10; X11; X12; X13; X14; X15; X16; X17;
                    X28; X29; X30; X31]

let general_purpose = [X9; X18; X19; X20; X21; X22; X23; X24;
                       X25; X26; X27; X10; X11; X12; X13; X14;
                       X15; X16; X17; X28; X29; X30; X31]

let all_regs = [X0; X1; X2; X3; X4; X5; X6; X7; X8; X9;
                X10; X11; X12; X13; X14; X15; X16; X17; X18; X19;
                X20; X21; X22; X23; X24; X25; X26; X27; X28; X29;
                X30; X31]

let reg_no =
  function X0 -> 0 | X1 -> 1 | X2 -> 2 | X3 -> 3 | X4 -> 4 | X5 -> 5
           | X6 -> 6 | X7 -> 7 | X8 -> 8 | X9 -> 9 | X10 -> 10 | X11 -> 11
           | X12 -> 12 | X13 -> 13 | X14 -> 14 | X15 -> 15 | X16 -> 16
           | X17 -> 17 | X18 -> 18 | X19 -> 19 | X20 -> 20 | X21 -> 21
           | X22 -> 22 | X23 -> 23 | X24 -> 24 | X25 -> 25 | X26 -> 26
           | X27 -> 27 | X28 -> 28 | X29 -> 29 | X30 -> 30 | X31 -> 31

let zero = X0
let ra = X1
let sp = X2
let fp = X8
let retval = X10
let args = [X10; X11; X12; X13; X14; X15; X16; X17]

type imm = int
type label = string
type symbol = string

type rop = Add | Sub | Mul | Div | Xor | Or | And | Slt | Sltu
           | Sll | Srl | Sra
type iop = Addi | Xori | Ori | Andi | Slti | Sltiu | Lw | Jalr | Calr
           | Slli | Srli | Srai
type bop = Beq | Bne | Blt | Bge

(* Type parameter allows us to represent both:
 * - LLVM.Ast.label inst: Assembly code with symbolic labels
 * - int inst: Linked RISC-V with numeric offsets
 *)

type 'a inst =
  | Label of 'a
  | R of rop * reg * reg * reg (* opcode, rd, rs1, rs2 *)
  | I of iop * reg * reg * imm (* opcode, rd, rs1, imm *)
  | Sw of reg * reg * imm (* M[rs1 + imm] = rs2 *)
  | B of bop * reg * reg * 'a (* opcode, rs1, rs2, dest *)
  | Jal of reg * 'a (* rd, dest *)
  | Lui of reg * imm
  | StoreLabel of reg * symbol
  | LoadLabel of reg * symbol
  | LoadAddress of reg * symbol
