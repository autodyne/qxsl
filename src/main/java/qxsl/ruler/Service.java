/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.util.List;

import qxsl.model.Item;

/**
 * 交信記録ソフトを支援する機能を提供します。
 * 交信記録は規約側で管理する必要があります。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/10/28
 */
public abstract class Service extends Counter {
	private final String name;

	/**
	 * 指定された名前のサービスを構築します。
	 *
	 *
	 * @param name 名前
	 */
	public Service(String name) {
		this.name = name;
	}

	/**
	 * サービスの名前を返します。
	 *
	 *
	 * @return サービスの名前
	 */
	@Override
	public final String getName() {
		return name;
	}

	/**
	 * 交信記録の追加の要求を受け付けます。
	 *
	 *
	 * @param item 交信記録
	 */
	public abstract void onInsertRequest(Item item);

	/**
	 * 交信記録の削除の要求を受け付けます。
	 *
	 *
	 * @param item 交信記録
	 */
	public abstract void onDeleteRequest(Item item);

	/**
	 * 交信記録の総得点の計算の要求を受け付けます。
	 *
	 *
	 * @param list 交信記録
	 *
	 * @return 総得点
	 */
	public final int onTotalRequest(List<Item> list) {
		return summarize(list).total();
	}

	/**
	 * 交信記録の有効性の判定の要求を受け付けます。
	 *
	 *
	 * @param item 交信記録
	 *
	 * @return 素点
	 */
	public final int onScoreRequest(Item item) {
		return verify(item).score();
	}

	/**
	 * 交信記録のマルチの抽出の要求を受け付けます。
	 *
	 *
	 * @param item 交信記録
	 *
	 * @return 抽出した文字列
	 */
	public abstract String onEntityRequest(Item item);

	/**
	 * 交信記録の書式のリストの要求を受け付けます。
	 *
	 *
	 * @return 書式のリストを表す文字列
	 */
	public abstract String onFormatRequest();

	/**
	 * 交信記録のバイト列の書き込みの要求を受け付けます。
	 *
	 *
	 * @param list 交信記録
	 *
	 * @return バイト列
	 */
	public abstract byte[] onEncodeRequest(List<Item> list);

	/**
	 * 交信記録のバイト列の読み取りの要求を受け付けます。
	 *
	 *
	 * @param data バイト列
	 *
	 * @return 交信記録
	 */
	public abstract List<Item> onDecodeRequest(byte[] data);

	/**
	 * 交信記録のファイルの書き込みの要求を受け付けます。
	 *
	 *
	 * @param path ファイルの場所
	 * @param form ファイルの書式
	 */
	public abstract void onExportRequest(String path, String form);

	/**
	 * 交信記録のファイルの読み取りの要求を受け付けます。
	 *
	 *
	 * @param path ファイルの場所
	 * @param form ファイルの書式
	 */
	public abstract void onImportRequest(String path, String form);
}
