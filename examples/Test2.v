(** * Preface *)

(** To see how this definition mechanism works, let's start with
    a very simple example.  The following [declaration] tells Coq that
    we are defining a new set of data values -- a _type_. *)

Inductive day : Type :=
  | monday : day
  | tuesday : day
  | wednesday : day
  | thursday : day
  | friday : day
  | saturday : day
  | sunday : day.
