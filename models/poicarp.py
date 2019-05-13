from problog.tasks.dtproblog import dtproblog
from problog.program import PrologString
from problog.program import PrologFile

program = PrologFile("models/poicarp.pl")
decisions, score, statistics = dtproblog(program)

for name, value in decisions.items():
    print ('%s: %s' % (name, value))

print score
print statistics
