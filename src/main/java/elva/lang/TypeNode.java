/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.lang;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.LinkedList;
import java.util.List;

import elva.warn.ElvaRuntimeException;

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
	 *
	 * @param value 型
	 */
	public TypeNode(Class<?> value) {
		this.value = value;
	}

	/**
	 * この式の値を処理系の外部に渡す際に使用します。
	 *
	 *
	 * @return 値
	 */
	@Override
	public final Class<?> value() {
		return value;
	}

	/**
	 * このアトムを表す文字列を返します。
	 *
	 *
	 * @return 文字列による式の表現
	 */
	@Override
	public final String toString() {
		return value.getCanonicalName();
	}

	/**
	 * このアトムからハッシュ値を計算します。
	 *
	 *
	 * @return ハッシュ値
	 */
	@Override
	public final int hashCode() {
		return value.hashCode();
	}

	/**
	 * このアトムと指定された値を比較します。
	 *
	 *
	 * @param sexp 比較対象の値
	 *
	 * @return 等価の場合は真
	 */
	@Override
	public final boolean equals(Object atom) {
		if(!TypeNode.class.isInstance(atom)) return false;
		return ((TypeNode) atom).value.equals(this.value);
	}

	/**
	 * 指定された値に対して型情報を返します。
	 *
	 *
	 * @param value 値
	 *
	 * @return 型情報
	 */
	public static final TypeNode of(NodeBase value) {
		return new TypeNode(value.value().getClass());
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
	 * 指定された名前を持つフィールドを検索して返します。
	 *
	 *
	 * @param name フィールドの名前
	 *
	 * @return フィールド
	 *
	 * @throws ElvaRuntimeException 未定義の場合
	 */
	public final Field getField(String name) {
		try {
			return value.getField(name);
		} catch (NoSuchFieldException ex) {
			throw new ElvaRuntimeException(ex);
		}
	}

	/**
	 * この型が備えるコンストラクタを全て検索して返します。
	 *
	 *
	 * @return コンストラクタのリスト
	 *
	 * @throws ElvaRuntimeException 未定義の場合
	 */
	public final List<Constructor> getConstructors() {
		final var list = value.getConstructors();
		if(list.length > 0) return List.of(list);
		final var msg = "no %s constructor found";
		throw new ElvaRuntimeException(msg, this);
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
	 * 指定された名前を持つメソッドを全て検索して返します。
	 *
	 *
	 * @param name メソッドの名前
	 *
	 * @return メソッドのリスト
	 *
	 * @throws ElvaRuntimeException 未定義の場合
	 */
	public final List<Method> getMethods(String name) {
		final var list = new LinkedList<Method>();
		for(var m: value.getMethods()) {
			if(m.getName().equals(name)) list.add(m);
		}
		if(!list.isEmpty()) return list;
		final var msg = "no such method found: '%s.%s'";
		throw new ElvaRuntimeException(msg, this, name);
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
