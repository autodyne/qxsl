/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.util.HashMap;
import java.util.Map;

import qxsl.model.Item;

/**
 * コンテストの総得点の計算手順を記憶します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/11/15
 */
public final class Promise extends Section {
	private final Section form;
	private final Map<Item, Message> vMap;
	private final Map<Item, Element> uMap;
	private final Map<Item, Element> eMap;

	/**
	 * 指定された計算手順を記憶します。
	 *
	 *
	 * @param form 計算手順
	 */
	public Promise(Section form) {
		this.form = form;
		this.vMap = new HashMap<>();
		this.uMap = new HashMap<>();
		this.eMap = new HashMap<>();
	}

	/**
	 * 規約が参照する変数値を返します。
	 *
	 *
	 * @param name 変数の名前
	 *
	 * @return 変数の値
	 *
	 * @since 2020/09/27
	 */
	@Override
	public final Object get(String name) {
		return form.get(name);
	}

	/**
	 * 部門の名前を返します。
	 *
	 *
	 * @return 名前
	 */
	@Override
	public final String name() {
		return form.name();
	}

	/**
	 * 部門の番号を返します。
	 *
	 *
	 * @return 番号
	 */
	@Override
	public final String code() {
		return form.code();
	}

	/**
	 * 指定された交信記録の妥当性を検査します。
	 *
	 *
	 * @param item 検査対象の交信記録
	 *
	 * @return 承認された場合はtrue
	 */
	@Override
	public final Message verify(Item item) {
		return vMap.computeIfAbsent(item, form::verify);
	}

	/**
	 * 指定された交信記録の識別子を発行します。
	 *
	 *
	 * @param item 検査対象の交信記録
	 *
	 * @return 重複を除くための識別子
	 */
	@Override
	public final Element unique(Item item) {
		return uMap.computeIfAbsent(item, form::unique);
	}

	/**
	 * 指定された交信記録のマルチを発行します。
	 *
	 *
	 * @param item 検査対象の交信記録
	 *
	 * @return 総得点を計算する識別子の配列
	 */
	@Override
	public final Element entity(Item item) {
		return eMap.computeIfAbsent(item, form::entity);
	}

	/**
	 * 指定された集計結果の総得点を計算します。
	 *
	 *
	 * @param items 集計結果
	 *
	 * @return 総得点
	 *
	 * @since 2020/02/26
	 */
	@Override
	public final int result(Summary items) {
		return form.result(items);
	}
}
