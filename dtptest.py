from problog.tasks.dtproblog import dtproblog
from problog.program import PrologString
from problog.program import PrologFile

# model = """
#     0.3::rain.
#     0.5::wind.
#     ?::umbrella.
#     ?::raincoat.
#
#     broken_umbrella :- umbrella, rain, wind.
#     dry :- rain, raincoat.
#     dry :- rain, umbrella, not broken_umbrella.
#     dry :- not(rain).
#
#     utility(broken_umbrella, -40).
#     utility(raincoat, -20).
#     utility(umbrella, -2).
#     utility(dry, 60).
# """

program = PrologFile("dtptest.pl")
decisions, score, statistics = dtproblog(program)

for name, value in decisions.items():
    print ('%s: %s' % (name, value))

print score
print statistics
