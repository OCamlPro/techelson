[%%version 0.405] 

type storage = {
  admins : (string, address) map ;
  (* Unused in this example. *)
  nus : (string, (address * tez * UnitContract.instance)) map ;
}

let admin_check (storage : storage) (name : string) (a : address) : unit =
  match Map.find name storage.admins with
  | None -> failwith "only admins can perform administrative tasks"
  | Some address ->
      if address <> a then
        failwith "illegal access to admin account"

let%entry add_admin
    ((admin_name, nu_name, nu_address) : string * string * address)
    (storage : storage)
  : operation list * storage 
=
  admin_check storage admin_name (Current.sender ()); 
  let storage =
    storage.admins <- Map.update nu_name (Some nu_address) storage.admins
  in
  [], storage