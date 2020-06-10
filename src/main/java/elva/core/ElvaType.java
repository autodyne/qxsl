/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.core;

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * LISP処理系で使用される型情報を表す専用のアトムの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/06/02
 */
public final class ElvaType extends ElvaAtom<Class<?>> {
	private final Class<?> value;

	/**
	 * 指定された型のアトムを構築します。
	 *
	 * @param value 型
	 */
	public ElvaType(Class<?> value) {
		this.value = value;
	}

	/**
	 * この型情報の値を返します。
	 *
	 * @return 値
	 */
	@Override
	public final Class<?> value() {
		return value;
	}

	/**
	 * この型情報とオブジェクトを比較します。
	 * 同じ内容の型情報であれば真を返します。
	 *
	 * @param type 比較対象のオブジェクト
	 * @return 同じ内容の型情報のみtrue
	 */
	@Override
	public final boolean equals(Object type) {
		if(ElvaType.class.isInstance(type)) {
			final Object val = ((ElvaType) type).value;
			return Objects.equals(val, this.value);
		} else return false;
	}

	/**
	 * この型情報の文字列による表現を返します。
	 *
	 * @return 文字列
	 */
	@Override
	public final String toString() {
		return String.format("(type %s)", value);
	}

	/**
	 * 指定された名前のフィールドを返します。
	 *
	 * @param name 名前
	 * @return フィールド
	 */
	public final Field getField(String name) {
		try {
			return value.getField(name);
		} catch (NoSuchFieldException ex) {
			return null;
		}
	}

	/**
	 * 指定された名前のメソッドを返します。
	 *
	 * @param name 名前
	 * @return メソッド
	 */
	public final List<Method> getMethods(String name) {
		final var list = new ArrayList<Method>();
		for(var m: value.getMethods()) {
			if(m.getName().equals(name)) list.add(m);
		}
		return list;
	}

	/**
	 * このクラスのコンストラクタを返します。
	 *
	 * @return コンストラクタ
	 */
	public final List<Constructor<?>> getConstructors() {
		return Arrays.asList(value.getConstructors());
	}

	/**
	 * 処理系の内外における暗黙的な型変換を定めます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/10
	 */
	public static final class Type implements Implicit {
		@Override
		public final boolean support(Object value) {
			return value instanceof Class;
		}

		@Override
		public final ElvaNode encode(Object value) {
			return new ElvaType((Class<?>) value);
		}
	}
}
