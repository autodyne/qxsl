import 'java.io.InputStreamReader'
import 'qxsl.ruler.RuleKit'

RULER = RuleKit.load('elva')
codes = RuleKit.java_class.resource_as_stream('allja1.lisp')
RULES = RULER.contest(InputStreamReader.new(codes, 'UTF-8'))
codes.close
RULES
