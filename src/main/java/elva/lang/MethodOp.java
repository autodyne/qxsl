/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import elva.warn.ElvaRuntimeException;

/**
 * LISP処理系からメソッドを参照する演算子の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/07/28
 */
public final class MethodOp extends FormBase {
	private static final int MAX = 256;
	private final Method body;

	/**
	 * 指定された式でメソッド参照を生成します。
	 *
	 *
	 * @param name メソッドの名前
	 *
	 * @param list 引数の型の配列
	 *
	 * @throws ElvaRuntimeException 何らかの例外
	 */
	public MethodOp(NameNode name, ListBase list) {
		final var type = list.head().type();
		final var pars = list.tail().cast(Class.class);
		this.body = type.getMethod(name.toString(), pars);
	}

	/**
	 * このメソッドが静的なメソッドであるか確認します。
	 *
	 * @return 文字列による式の表現
	 */
	public final boolean isStatic() {
		return Modifier.isStatic(body.getModifiers());
	}

	/**
	 * このメソッドが可変長引数のメソッドか確認します。
	 *
	 *
	 * @return 可変長引数なら真
	 */
	@Override
	public final boolean isVarArgs() {
		return body.isVarArgs();
	}

	/**
	 * このメソッドが取る引数の最小の個数を返します。
	 *
	 *
	 * @return 最小限の引数の個数
	 */
	@Override
	public final int getMinimumArgumentLength() {
		final int obj = isStatic()? 0: 1;
		final int cnt = body.getParameterCount();
		return obj + (isVarArgs()? cnt - 1: cnt);
	}

	/**
	 * このメソッドが取る引数の最大の個数を返します。
	 *
	 *
	 * @return 最大限の引数の個数
	 */
	@Override
	public final int getMaximumArgumentLength() {
		final int obj = isStatic()? 0: 1;
		final int cnt = body.getParameterCount();
		return obj + (isVarArgs()? MAX - 1: cnt);
	}

	/**
	 * このメソッドを表す文字列を返します。
	 *
	 *
	 * @return 文字列による式の表現
	 */
	@Override
	public final String toString() {
		return body.toGenericString();
	}

	/**
	 * 指定された対象と実引数でメソッドを実行します。
	 *
	 *
	 * @param obj オブジェクト
	 * @param seq 実引数の配列
	 *
	 * @return 返り値
	 *
	 * @throws ElvaRuntimeException 評価で発生した例外
	 */
	private final Object invoke(Object obj, Object...seq) {
		try {
			return body.invoke(obj, seq);
		} catch (InvocationTargetException ex) {
			throw new ElvaRuntimeException(ex);
		} catch (IllegalAccessException ex) {
			throw new ElvaRuntimeException(ex);
		}
	}

	/**
	 * 指定された実引数と評価器に対し、返り値を求めます。
	 *
	 *
	 * @param args 実引数
	 * @param eval 評価器
	 *
	 * @return 返り値
	 *
	 * @throws ElvaRuntimeException 評価で発生した例外
	 */
	@Override
	public final Object apply(ListBase args, ElvaEval eval) {
		final var seq = args.map(eval);
		final var par = body.getParameterTypes();
		final var obj = isStatic()? null: seq.head().value();
		final var arg = seq.drop(isStatic()? 0: 1).cast(par);
		return invoke(obj, arg);
	}
}
