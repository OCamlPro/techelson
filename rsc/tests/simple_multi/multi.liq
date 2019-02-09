[%%version 0.405] 

type storage = {
  admins : (string, address) map ;
  users : (string, (address * tez * UnitContract.instance)) map ;
}

let admin_check (storage : storage) (name : string) (a : address) : unit =
  match Map.find name storage.admins with
  | None -> failwith "only admins can perform administrative tasks"
  | Some address ->
      if address <> a then
        failwith "illegal access to admin account"

let%entry add_admin
    ((admin_name, user_name, user_address) : string * string * address)
    (storage : storage)
  : operation list * storage 
=
  admin_check storage admin_name (Current.sender ()); 
  let storage = storage.admins <- Map.update user_name (Some user_address) storage.admins in
  [], storage
  
let%entry rm_admin (admin_name, user_name : string * string) (storage : storage) =
  admin_check storage admin_name (Current.sender ());
  let storage = storage.admins <- Map.update user_name None storage.admins in
  [], storage
    
let%entry add_user ((admin_name, user_name, user, c) : string * string * address * UnitContract.instance) (storage : storage) =
  admin_check storage admin_name (Current.sender ());
  if Map.mem user_name storage.users then (
    failwith "username already taken"
  );
  let data = Some (user, 0tz, c) in
  let storage = storage.users <- Map.update user_name data storage.users in
  [], storage
  
let data_of (storage : storage) (name : string) (user : address) : tez * UnitContract.instance =
  match Map.find name storage.users with
  | None -> failwith "unknown user"
  | Some (address, tez, c) ->
      if user <> address then
        failwith "illegal access to account"
      else (tez, c)

let%entry deposit (name : string) (storage : storage) =
  let user = Current.sender () in
  let money, c = data_of storage name user in
  let amount = Current.amount () in
  let nu_data = Some (user, money + amount, c) in
  [], storage.users <- Map.update name nu_data storage.users 
          
          
let%entry withdraw (name : string) (storage : storage) =
  let user = Current.sender () in
  let money, c = data_of storage name user in
  let amount = Current.amount () in
  if amount > money then
    failwith "insufficient balance"
  else (
    let nu_data = Some (user, money - amount, c) in
    [], storage.users <- Map.update name nu_data storage.users
  )
        
let%entry drain (name : string) (storage : storage) =
  let user = Current.sender () in
  let money, c = data_of storage name user in
  let storage = storage.users <- Map.update name None storage.users in
  let ops = [Contract.call ~dest:c ~amount:money ~parameter:()] in
  ops, storage

let%test add_client () =
  let key = "ed0f4ad01691fddbb5659349b80b4bee9" in
  let manager = Crypto.hash_key key in
  let delegatable = true in
  let spendable = false in

  let storage = () in
  let admin_op, admin_address =
    Account.create ~manager ~delegatable ~delegate:None ~amount:0tz
  in

  let storage = () in
  let client_op, client_address =
    Account.create ~manager ~delegatable ~delegate:None ~amount:0tz
  in

  let admins = Map.empty |> Map.add "Muriel" in
  let clients = Map.empty in
  let storage = (admins, clients) in

  let contract_op, contract_address =
    Contract.create ~storage ~manager ~spendable ~delegatable ~delegate:None ~amount:0tz Multi
  in

  Test.apply_ops [ admin_op ; client_op ; contract_op ] ;

  


