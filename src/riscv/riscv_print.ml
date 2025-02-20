open Riscv_ast

   (*
let string_of_reg = function
  | X0 -> "x0"
  | X1 -> "x1"
  | X2 -> "x2"
  | X3 -> "x3"
  | X4 -> "x4"
  | X5 -> "x5"
  | X6 -> "x6"
  | X7 -> "x7"
  | X8 -> "x8"
  | X9 -> "x9"
  | X10 -> "x10"
  | X11 -> "x11"
  | X12 -> "x12"
  | X13 -> "x13"
  | X14 -> "x14"
  | X15 -> "x15"
  | X16 -> "x16"
  | X17 -> "x17"
  | X18 -> "x18"
  | X19 -> "x19"
  | X20 -> "x20"
  | X21 -> "x21"
  | X22 -> "x22"
  | X23 -> "x23"
  | X24 -> "x24"
  | X25 -> "x25"
  | X26 -> "x26"
  | X27 -> "x27"
  | X28 -> "x28"
  | X29 -> "x29"
  | X30 -> "x30"
  | X31 -> "x31"
    *)
let string_of_reg = function
  | X0 -> "zero"
  | X1 -> "ra"
  | X2 -> "sp"
  | X3 -> "gp"
  | X4 -> "tp"
  | X5 -> "t0"
  | X6 -> "t1"
  | X7 -> "t2"
  | X8 -> "fp"
  | X9 -> "s1"
  | X10 -> "a0"
  | X11 -> "a1"
  | X12 -> "a2"
  | X13 -> "a3"
  | X14 -> "a4"
  | X15 -> "a5"
  | X16 -> "a6"
  | X17 -> "a7"
  | X18 -> "s2"
  | X19 -> "s3"
  | X20 -> "s4"
  | X21 -> "s5"
  | X22 -> "s6"
  | X23 -> "s7"
  | X24 -> "s8"
  | X25 -> "s9"
  | X26 -> "s10"
  | X27 -> "s11"
  | X28 -> "t3"
  | X29 -> "t4"
  | X30 -> "t5"
  | X31 -> "t6"
   
let string_of_rop =
  function
    Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Div -> "div"
  | Xor -> "xor"
  | Or -> "or"
  | And -> "and"
  | Slt -> "slt"
  | Sltu -> "sltu"
  | Sll -> "sll"
  | Srl -> "srl"
  | Sra -> "sra"

let string_of_iop =
  function
    Addi -> "addi"
  | Xori -> "xori"
  | Ori -> "ori"
  | Andi -> "andi"
  | Slti -> "slti"
  | Sltiu -> "sltiu"
  | Lw -> "lw"
  | Jalr -> "jalr"
  | Calr -> "calr"
  | Slli -> "slli"
  | Srli -> "srli"
  | Srai -> "srai"

let string_of_bop =
  function
    Beq -> "beq"
  | Bne -> "bne"
  | Blt -> "blt"
  | Bge -> "bge"

let print_inst (string_of_label: 'a -> string) f (i: 'a inst) =
  match i with
  | Label l -> Format.fprintf f "%s:\n" (string_of_label l)
  | R (op, rd, rs1, rs2) ->
     Format.fprintf f "  %s %s,%s,%s\n"
       (string_of_rop op)
       (string_of_reg rd)
       (string_of_reg rs1)
       (string_of_reg rs2)
  | I (Lw, rd, rs1, im) ->
     Format.fprintf f "  lw %s,%d(%s)\n"
       (string_of_reg rd)
       im
       (string_of_reg rs1)
  | I (op, rd, rs1, im) ->
     Format.fprintf f "  %s %s,%s,%d\n"
       (string_of_iop op)
       (string_of_reg rd)
       (string_of_reg rs1)
       im
  | Sw (rs1, rs2, im) ->
     Format.fprintf f "  sw %s,%d(%s)\n"
       (string_of_reg rs2)
       im
       (string_of_reg rs1)
  | B (op, rs1, rs2, l) ->
     Format.fprintf f "  %s %s,%s,%s\n"
       (string_of_bop op)
       (string_of_reg rs1)
       (string_of_reg rs2)
       (string_of_label l)
  | Jal (r, l) ->
     Format.fprintf f "  jal %s,%s\n"
       (string_of_reg r)
       (string_of_label l)
  | Lui (r, im) ->
     Format.fprintf f "  lui %s,%u\n"
       (string_of_reg r)
       im
  | StoreLabel (r, s) ->
     Format.fprintf f "  sw %s,%s,x3\n"
       (string_of_reg r)
       s
  | LoadAddress (r, s) ->
     Format.fprintf f "  la %s,%s\n"
       (string_of_reg r)
       s
  | LoadLabel (r, s) ->
     Format.fprintf f "  lw %s,%s\n"
       (string_of_reg r)
       s

let print_glob f (n, v) =
  Format.fprintf f "%s:\n  .word %d\n\n" n v
    
let print_prog f (insts, funcs, globs) =
  Format.fprintf f ".text\n";
  List.iter (fun s -> Format.fprintf f ".globl %s\n" s) funcs;
  Format.fprintf f "\n";
  List.iter (print_inst (fun s -> s) f) insts;
  Format.fprintf f "\n\n.data\n\n";
  List.iter (print_glob f) globs

let print_located_prog f (l: int inst list) =
  List.iter (print_inst string_of_int f) l
