module Message = struct
  type t =
    | IO_error
    | Parse_error
    | NoVar_error
    | Type_error
    (* maybe need more, e.g. distinguish unification and assign error *)
    | TODO
  [@@deriving show]

  let default_severity : t -> Asai.Diagnostic.severity = function
    | IO_error -> Error
    | Parse_error -> Error
    | NoVar_error -> Error
    | Type_error -> Error
    | TODO -> Warning

  let short_code : t -> string = show
end

include Asai.Reporter.Make (Message)
