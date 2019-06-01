/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package elva;

import javax.script.ScriptException;

import static elva.ElvaScriptEngine.Lisp;
import static elva.ElvaScriptEngine.Seq;

/**
 * LISP処理系の関数やマクロはこのインターフェースを実装します。
 *
 *
 * @author Journal of Hamradio Informatics
 *
 * @since 2019/05/15
 */
@FunctionalInterface
public interface Function {
	/**
	 * 指定された実引数と評価器に対し、返り値を求めます。
	 *
	 * @param args 実引数
	 * @param eval 評価器
	 * @return 返り値
	 * @throws ScriptException 評価により生じた例外
	 */
	public Object apply(Seq args, Lisp eval) throws ScriptException;
}
