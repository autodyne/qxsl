/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.ruler;

import java.util.ArrayList;
import java.util.List;
import javax.script.ScriptException;
import qxsl.model.Item;

/**
 * コンテストの部門の実装は{@link Section}クラスを実装します。
 * 
 * 
 * @author Journal of Hamradio Informatics
 *
 * @since 2016/11/25
 */
public abstract class Section {
	/**
	 * 部門名を返します。
	 *
	 * @return 部門名
	 */
	public abstract String getName();

	/**
	 * UIで表示するために部門名を返します。
	 *
	 * @return {@link #getName()}と同等
	 */
	@Override
	public final String toString() {
		return getName();
	}

	/**
	 * 指定された{@link Item}が通過するか確認します。
	 *
	 * @param item 検査対象の交信記録
	 * @return 承認された場合はtrue
	 * 
	 * @throws ScriptException スクリプトの実行で発生した何らかの例外
	 */
	public abstract Message validate(Item item) throws ScriptException;

	/**
	 * 指定された交信記録に対して有効な交信を数え上げて得点を計算します。
	 *
	 * @param items 交信記録
	 * @return 得点計算の結果
	 * 
	 * @since 2019/05/16
	 * 
	 * @throws ScriptException スクリプトの実行で発生した何らかの例外
	 */
	public Summary summarize(List<Item> items) throws ScriptException {
		final List<Success> accepted = new ArrayList<>();
		final List<Failure> rejected = new ArrayList<>();
		for(Item item: items) {
			final Message msg = this.validate(item);
			if(msg instanceof Success) accepted.add((Success) msg);
			if(msg instanceof Failure) rejected.add((Failure) msg);
		}
		return new Summary(accepted, rejected);
	}
}
