/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.draft;

/**
 * 交信を行なった運用者の個人の名前を表現します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class Name extends Qxsl<String> {
	private final String name;

	/**
	 * 運用者名を指定して属性を構築します。
	 *
	 *
	 * @param name 運用者名
	 */
	public Name(String name) {
		super(NAME);
		this.name = name;
	}

	@Override
	public final String value() {
		return name;
	}
}
