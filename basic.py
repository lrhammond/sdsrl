from pydc import HYPE

hype = HYPE("basic.pl", 500)
result = hype.plan_step(
    # "[observation(posX(obj))~=(0),observation(posY(obj))~=(0),observation(posX(obj2))~=(50),observation(posY(obj2))~=(50)]",
    # "[observation(posX(obj))~=(0),observation(posY(obj))~=(0)]",
    "[observation(posX(obj))~=(0),observation(posY(obj))~=(0),observation(shape(obj))~=(square),observation(posX(obj2))~=(50),observation(posY(obj2))~=(50),observation(shape(obj2))~=(round)]",
    # "[]",
    250, max_horizon=20,
    used_horizon=10,
    use_abstraction=False
)
best_action = result["best_action"]
stop = result["stop"]
print best_action
