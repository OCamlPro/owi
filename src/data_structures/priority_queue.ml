module Make (Prio : Map.OrderedType) = struct
  type 'a t = (Prio.t * 'a) Dynarray.t

  let empty = Dynarray.create

  let is_empty h = Dynarray.length h = 0

  let rec move_up (h : 'a t) x i =
    if i = 0 then Dynarray.set h i x
    else
      let fi = (i - 1) / 2 in
      let y = Dynarray.get h fi in
      if Prio.compare (fst y) (fst x) > 0 then begin
        Dynarray.set h i y;
        move_up h x fi
      end
      else Dynarray.set h i x

  let push x h =
    let n = Dynarray.length h in
    Dynarray.add_last h x;
    move_up h x n

  let min (h : 'a t) l r =
    let xr = Dynarray.get h r in
    let xl = Dynarray.get h l in
    if Prio.compare (fst xr) (fst xl) < 0 then r else l

  let smallest_node h x i =
    let l = (2 * i) + 1 in
    let n = Dynarray.length h in
    if l >= n then i
    else
      let r = l + 1 in
      let j = if r < n then min h l r else l in
      if Prio.compare (fst (Dynarray.get h j)) (fst x) < 0 then j else i

  let rec move_down h x i =
    let j = smallest_node h x i in
    if j = i then Dynarray.set h i x
    else begin
      Dynarray.set h i (Dynarray.get h j);
      move_down h x j
    end

  let pop h =
    let n = Dynarray.length h in
    if n = 0 then None
    else
      let y = Dynarray.get h 0 in
      match Dynarray.pop_last_opt h with
      | None -> None
      | Some x ->
        if n > 1 then move_down h x 0;
        Some (snd y)
end
