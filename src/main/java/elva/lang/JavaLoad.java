/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

import elva.warn.ElvaRuntimeException;

/**
 * LISP処理系からフィールドを参照する演算子の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/22
 */
public final class JavaLoad extends FormBase {
	private final Field body;

	/**
	 * 指定されたフィールドを参照します。
	 *
	 *
	 * @param body フィールドの実体
	 */
	public JavaLoad(Field body) {
		this.body = body;
	}

	/**
	 * 指定された名前と型のフィールドを参照します。
	 *
	 *
	 * @param name フィールドの名前
	 * @param type 引数の型
	 *
	 * @throws ElvaRuntimeException 何らかの例外
	 */
	public JavaLoad(NameNode name, TypeNode type) {
		this.body = type.getField(name.toString());
	}

	/**
	 * この演算子の実体を返します。
	 *
	 *
	 * @return 演算子の実体
	 */
	@Override
	public final Field value() {
		return body;
	}

	/**
	 * この演算子が静的なフィールドであるか確認します。
	 *
	 *
	 * @return 文字列による式の表現
	 */
	public final boolean isStatic() {
		return Modifier.isStatic(body.getModifiers());
	}

	/**
	 * この演算子が可変長引数のフィールドか確認します。
	 *
	 *
	 * @return 常に偽
	 */
	@Override
	public final boolean isVarArgs() {
		return false;
	}

	/**
	 * このフィールドが取る引数の最小の個数を返します。
	 *
	 *
	 * @return 最小限の引数の個数
	 */
	@Override
	public final int getMinimumArgumentLength() {
		return isStatic()? 0: 1;
	}

	/**
	 * このフィールドが取る引数の最大の個数を返します。
	 *
	 *
	 * @return 最大限の引数の個数
	 */
	@Override
	public final int getMaximumArgumentLength() {
		return isStatic()? 0: 1;
	}

	/**
	 * このフィールドを表す文字列を返します。
	 *
	 *
	 * @return 文字列による式の表現
	 */
	@Override
	public final String toString() {
		return body.toGenericString();
	}

	/**
	 * 指定された対象のフィールドを参照します。
	 *
	 *
	 * @param obj 値
	 *
	 * @return 返り値
	 *
	 * @throws ElvaRuntimeException 評価で発生した例外
	 */
	public final Object invoke(NodeBase obj) {
		try {
			return body.get(obj.value());
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
		return invoke(eval.apply(args.head()));
	}
}
