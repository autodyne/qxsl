/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package elva.core;

import java.util.Objects;

/**
 * LISP処理系で使用される真偽値のための専用のアトムの実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/03/11
 */
public final class ElvaBool extends ElvaAtom<Boolean> {
	private final boolean value;

	/**
	 * 真を表すアトムです。
	 */
	public static final ElvaBool T = new ElvaBool(true);

	/**
	 * 偽を表すアトムです。
	 */
	public static final ElvaBool F = new ElvaBool(false);

	/**
	 * 指定された値で真偽値を構築します。
	 *
	 * @param value 値
	 */
	private ElvaBool(boolean value) {
		this.value = value;
	}

	/**
	 * この真偽値の値を返します。
	 *
	 * @return 値
	 */
	@Override
	public final Boolean value() {
		return value;
	}

	/**
	 * この真偽値とオブジェクトを比較します。
	 * 同じ内容の真偽値であれば真を返します。
	 *
	 * @param atom 比較対象のオブジェクト
	 * @return 同じ内容の真偽値のみtrue
	 */
	@Override
	public final boolean equals(Object atom) {
		if(!ElvaBool.class.isInstance(atom)) return false;
		else return ((ElvaBool) atom).value == this.value;
	}

	/**
	 * この真偽値を式として表す真偽値を返します。
	 *
	 * @return 真偽値
	 */
	@Override
	public final String toString() {
		return value? "#t": "#f";
	}

	/**
	 * 処理系の内外における暗黙的な型変換を定めます。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2020/06/10
	 */
	public static final class BOOL implements Implicit {
		@Override
		public final boolean support(Object value) {
			return value instanceof Boolean;
		}

		@Override
		public final ElvaNode encode(Object value) {
			return (boolean) value? ElvaBool.T: ElvaBool.F;
		}
	}
}
