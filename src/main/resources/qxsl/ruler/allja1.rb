import 'java.io.InputStreamReader'
import 'qxsl.ruler.RuleKit'

ruler = RuleKit.load('elva')
codes = RuleKit.java_class.resource_as_stream('allja1.lisp')
$rule = ruler.contest(InputStreamReader.new(codes, 'UTF-8'))
codes.close()
$rule
