// choices are weird

open System

let div num den = 
    num / den

div 15 3

let safeDiv num den = 
    if den = 0. then
        Choice1Of2 "dive by zero is undefined"
    else
        Choice2Of2 (num / den)

safeDiv 15. 3.
safeDiv 15. 0.