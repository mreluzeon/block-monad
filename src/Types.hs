module Types where

import STMSet
import States

type Flower = (Int, Int)

type FlowerMap = STMSet Flower

type FlowerState = State Flower
