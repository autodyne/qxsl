/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import elva.warn.ElvaRuntimeException;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

/**
 * LISP処理系で使用される型情報を表す専用のアトムの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/02
 */
public final class TypeNode extends AtomBase<Class<?>> {
	private final Class<?> value;

	/**
	 * 指定された型のアトムを構築します。
	 *
	 * @param value 型
	 */
	public TypeNode(Class<?> value) {
		this.value = value;
	}

	@Override
	public final Class<?> value() {
		return value;
	}

	@Override
	public final String toString() {
		return value.toString();
	}

	@Override
	public final int hashCode() {
		return value.hashCode();
	}

	@Override
	public final boolean equals(Object atom) {
		if(!TypeNode.class.isInstance(atom)) return false;
		return ((TypeNode) atom).value.equals(this.value);
	}

	/**
	 * 処理系の内外における暗黙的な型変換を定めます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/10
	 */
	public static final class TYPE implements TypeRule {
		@Override
		public final boolean support(Object value) {
			return value instanceof Class;
		}

		@Override
		public final NodeBase encode(Object value) {
			return new TypeNode((Class<?>) value);
		}
	}

	/**
	 * 指定された引数型のコンストラクタを検索して返します。
	 *
	 *
	 * @param pars 引数の型の配列
	 *
	 * @return コンストラクタ
	 *
	 * @throws ElvaRuntimeException 未定義の場合
	 */
	public final Constructor getConstructor(Class<?>[] pars) {
		try {
			return value.getConstructor(pars);
		} catch (NoSuchMethodException ex) {
			throw new ElvaRuntimeException(ex);
		}
	}

	/**
	 * 指定された名前と引数型のメソッドを検索して返します。
	 *
	 *
	 * @param name メソッドの名前
	 * @param pars 引数の型の配列
	 *
	 * @return メソッド
	 *
	 * @throws ElvaRuntimeException 未定義の場合
	 */
	public final Method getMethod(String name, Class<?>[] pars) {
		try {
			return value.getMethod(name, pars);
		} catch (NoSuchMethodException ex) {
			throw new ElvaRuntimeException(ex);
		}
	}
}
