open Gfile

(* Parse a well-formatted file and print answer *)
val share_from_file: path -> path -> unit

(* Idem with a CSV file (also support coef. and complex shares, cf. readme) *)
val share_from_csv: path -> path -> unit