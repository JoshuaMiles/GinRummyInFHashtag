module ComputerPlayer

open Cards
open GinRummy

type Move = Gin | Knock | Continue

let averageAllOfThePossibleDeadwoods (deck:seq<Card>,currentHand:seq<Card>) =
    (deck |> Seq.map (fun card -> Deadwood (Seq.append (seq[card]) currentHand) ) |> Seq.sum ) / ((Seq.length deck))

let ComputerPickupDiscard computerHand topDiscard possibleDeck =
    let topDiscardDeadwood = Deadwood (Seq.append (seq[topDiscard]) computerHand )
    let averagedDeckDeadwood = averageAllOfThePossibleDeadwoods (possibleDeck |> Seq.filter (fun x -> not(x = topDiscard)), computerHand) 
    if (topDiscardDeadwood < averagedDeckDeadwood) then 
         true
    else
        false



let worstCard hand = 
    hand |> Seq.map (fun card -> (Deadwood (findLeftOver(hand,seq<Card>[card])), card)) |> Seq.minBy fst |> snd

let ComputerMove newHand =
    let currentDeadwood = Deadwood newHand
    let cardToRemove = worstCard newHand
    let newDeadwood = Deadwood (findLeftOver(newHand, seq<Card>[cardToRemove]))
    if (currentDeadwood = 0) then
        (Gin, None)
    elif (newDeadwood = 0) then
        (Gin, Some cardToRemove)
    elif(newDeadwood <= 10 ) then
        (Knock, Some cardToRemove)
    elif (currentDeadwood <= 10 ) then 
        (Knock, Some cardToRemove)
    else 
        (Continue, Some (worstCard newHand))
