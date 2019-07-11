/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package elva;

import java.io.Serializable;

/**
 * LISP処理系で使用される識別子の実装です。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2017/02/18
 */
public final class Symbol implements Serializable {
	private final String id;

	/**
	 * 名前を指定して識別子を生成します。
	 *
	 * @param id 名前
	 */
	public Symbol(String id) {
		this.id = id;
	}

	/**
	 * この識別子の名前を返します。
	 *
	 * @return 名前
	 */
	public final String toString() {
		return id;
	}

	/**
	 * この識別子のハッシュ値を返します。
	 *
	 * @return ハッシュ値
	 */
	public final int hashCode() {
		return id.hashCode();
	}

	/**
	 * この識別子とオブジェクトを比較します。
	 * 同じ名前の識別子であれば真を返します。
	 *
	 * @param obj 比較対象のオブジェクト
	 * @return 同じ名前の識別子のみtrue
	 */
	public final boolean equals(Object obj) {
		if (!(obj instanceof Symbol)) return false;
		return obj.toString().equals(toString());
	}
}
