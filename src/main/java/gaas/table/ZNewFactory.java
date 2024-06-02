/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package gaas.table;

import java.io.InputStream;
import java.io.OutputStream;

import qxsl.draft.Band;
import qxsl.draft.Mode;
import qxsl.draft.Watt;
import qxsl.table.TableDecoder;
import qxsl.table.TableEncoder;

/**
 * zLogの交信記録が従うZLOX書式の互換実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2022/06/22
 */
public final class ZNewFactory extends ZLogFactory {
	/**
	 * 書式を構築します。
	 */
	public ZNewFactory() {
		super("znew");
	}

	/**
	 * 指定された入力を読み込むデコーダを返します。
	 *
	 *
	 * @param is 交信記録を読み込む入力
	 *
	 * @return デコーダ
	 */
	@Override
	public final TableDecoder decoder(InputStream is) {
		return new ZNewDecoder(is);
	}

	/**
	 * 指定された出力に書き込むエンコーダを返します。
	 *
	 *
	 * @param os 交信記録を書き込む出力
	 *
	 * @return エンコーダ
	 */
	@Override
	public final TableEncoder encoder(OutputStream os) {
		return new ZNewEncoder(os);
	}

	/**
	 * この書式が対応する周波数帯の集合を返します。
	 *
	 *
	 * @return 周波数帯の集合
	 *
	 * @since 2024/06/02
	 */
	public static final FieldSet<Band> getBandSet() {
		final var set = new FieldSet<Band>("bands");
		set.add(new Band(    1900));
		set.add(new Band(    3500));
		set.add(new Band(    7000));
		set.add(new Band(   10000));
		set.add(new Band(   14000));
		set.add(new Band(   18000));
		set.add(new Band(   21000));
		set.add(new Band(   24000));
		set.add(new Band(   28000));
		set.add(new Band(   50000));
		set.add(new Band(  144000));
		set.add(new Band(  430000));
		set.add(new Band( 1200000));
		set.add(new Band( 2400000));
		set.add(new Band( 5600000));
		set.add(new Band(10000000));
		return set;
	}

	/**
	 * この書式が対応する通信方式の集合を返します。
	 *
	 *
	 * @return 通信方式の集合
	 *
	 * @since 2024/06/02
	 */
	public static final FieldSet<Mode> getModeSet() {
		final var set = new FieldSet<Mode>("modes");
		set.add(new Mode("CW"));
		set.add(new Mode("SSB"));
		set.add(new Mode("FM"));
		set.add(new Mode("AM"));
		set.add(new Mode("RTTY"));
		set.add(new Mode("FT4"));
		set.add(new Mode("FT8"));
		set.add(new Mode("Other"));
		return set;
	}

	/**
	 * この書式が対応する送信電力の集合を返します。
	 *
	 *
	 * @return 送信電力の集合
	 *
	 * @since 2024/06/02
	 */
	public static final FieldSet<Watt> getWattSet() {
		final var set = new FieldSet<Watt>("watts");
		set.add(new Watt("P"));
		set.add(new Watt("L"));
		set.add(new Watt("M"));
		set.add(new Watt("H"));
		return set;
	}
}
