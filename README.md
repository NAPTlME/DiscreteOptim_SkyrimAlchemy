# DiscreteOptim_SkyrimAlchemy
## This is a prototype

Problem: When starting a new save, the act of exposing ingredient effects for potions is tedious and personal preference is to avoid for repeated playthroughs. Current utilities (online/mobile) fail to provide a sufficient solution (not tailored to the player's custom inventory and might represent paths not feasable at the moment for the player), console commands are not preferred.

Solution: Create a discrete optimization problem that is able to recommend the potions that will unlock the most ingredient effects in the fewest potion creations (to save on materials). This solution should be tailored to the individual's current inventory.

Data Structure: A graph structure consisting of two vertex types (ingredients and effects). Ingredients contain properties regarding inventory count. Edges represent ingredient effects (each ingredient has 4 effects in this game). Edges contain a boolean property `Known` indicating if the effect for an ingredient has been revieled yet.
