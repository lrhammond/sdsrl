% Background knowledge
% person(denis).
% person(thiago).
% person(leliane).
% person(fabio).
people([denis, thiago, leliane, fabio])

trusts(thiago,leliane).
trusts(denis,leliane).
trusts(denis,fabio).
trusts(fabio,leliane).
trusts(leliane,fabio).
trusts(leliane,thiago).
