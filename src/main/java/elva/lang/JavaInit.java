/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package elva.lang;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import elva.warn.ElvaRuntimeException;

/**
 * LISP処理系からコンストラクタを参照する演算子の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/09/02
 */
public final class JavaInit extends FormBase {
	private static final int MAX = 256;
	private final Constructor body;

	/**
	 * 指定されたコンストラクタを参照します。
	 *
	 *
	 * @param body コンストラクタの実体
	 */
	public JavaInit(Constructor body) {
		this.body = body;
	}

	/**
	 * 指定された型のコンストラクタを参照します。
	 *
	 *
	 * @param type コンストラクタの型
	 * @param list 引数の型の配列
	 *
	 * @throws ElvaRuntimeException 何らかの例外
	 */
	public JavaInit(TypeNode type, ListBase list) {
		this(type.getConstructor(list.cast(Class.class)));
	}

	/**
	 * この演算子の実体を返します。
	 *
	 *
	 * @return 演算子の実体
	 */
	@Override
	public final Constructor value() {
		return body;
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
		final int cnt = body.getParameterCount();
		return isVarArgs()? cnt - 1: cnt;
	}

	/**
	 * このメソッドが取る引数の最大の個数を返します。
	 *
	 *
	 * @return 最大限の引数の個数
	 */
	@Override
	public final int getMaximumArgumentLength() {
		final int cnt = body.getParameterCount();
		return isVarArgs()? MAX: cnt;
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
		final var par = body.getParameterTypes();
		final var seq = args.map(eval).cast(par);
		try {
			return body.newInstance(seq);
		} catch (InvocationTargetException ex) {
			throw new ElvaRuntimeException(ex);
		} catch (IllegalAccessException ex) {
			throw new ElvaRuntimeException(ex);
		} catch (InstantiationException ex) {
			throw new ElvaRuntimeException(ex);
		}
	}
}
