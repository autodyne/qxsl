/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.ruler;

import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.List;

import qxsl.draft.*;
import qxsl.local.LocalCityBase;
import qxsl.local.LocalCityItem;
import qxsl.model.Item;

/**
 * コンテストの部門はこのクラスを継承します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2016/11/25
 */
public abstract class Section extends Library {
	/**
	 * 部門を構築します。
	 */
	public Section() {}

	/**
	 * 部門の名前を返します。
	 *
	 *
	 * @return 名前
	 */
	@Override
	public final String toString() {
		return name();
	}

	/**
	 * 部門の名前を返します。
	 *
	 *
	 * @return 名前
	 */
	public abstract String name();

	/**
	 * 部門の分類を返します。
	 *
	 *
	 * @return 分類
	 */
	public abstract String code();

	/**
	 * この部門が不参加部門であるか確認します。
	 *
	 *
	 * @return 不参加部門の場合は真
	 */
	public final boolean isAbsence() {
		return this instanceof Absence;
	}

	/**
	 * 指定された交信記録の妥当性を検査します。
	 *
	 *
	 * @param item 検査対象の交信記録
	 *
	 * @return 検証結果
	 */
	public abstract Message verify(Item item);

	/**
	 * 指定された交信記録の識別子を発行します。
	 *
	 *
	 * @param item 検査対象の交信記録
	 *
	 * @return 重複を除くための識別子
	 *
	 * @since 2020/11/02
	 */
	public abstract Element unique(Item item);

	/**
	 * 指定された交信記録のマルチを発行します。
	 *
	 *
	 * @param item 検査対象の交信記録
	 *
	 * @return 総得点を計算する識別子の配列
	 *
	 * @since 2020/11/02
	 */
	public abstract Element entity(Item item);

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
	public abstract int result(Summary items);

	/**
	 * 指定された交信記録のマルチを集計します。
	 *
	 *
	 * @param list 交信記録
	 *
	 * @return 得点計算の結果
	 *
	 * @since 2019/05/16
	 */
	public final Summary summarize(List<Item> list) {
		return new Summary(this, list);
	}

	/**
	 * 指定された得点分布で入賞局数を返します。
	 *
	 *
	 * @param scores 総得点の配列
	 *
	 * @return 入賞する参加局の数
	 *
	 * @since 2022/07/23
	 */
	public int getAwardLimit(int[] scores) {
		final double size = 0.1 * scores.length;
		return (int) Math.min(7, Math.ceil(size));
	}

	/**
	 * この部門に参加可能な運用場所を返します。
	 *
	 *
	 * @return 運用場所のリスト
	 *
	 * @since 2024/07/15
	 */
	public final LocalCityBase getCityBase() {
		return new LocalCityBase(getCityList());
	}

	/**
	 * この部門に参加可能な運用場所を返します。
	 *
	 *
	 * @return 運用場所のリスト
	 *
	 * @since 2022/06/22
	 */
	public abstract List<LocalCityItem> getCityList();

	/**
	 * 標準的な実装です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2024/10/19
	 */
	public static abstract class Default extends Section {
		private ZoneId zoneId = ZoneId.systemDefault();
		private LocalCityBase codes;
		private List<Integer> hours;
		private List<Band> bands;
		private List<Mode> modes;
		private String name;
		private String code;

		/**
		 * 指定された部門を構築します。
		 *
		 *
		 * @param name 部門の名前
		 * @param code 部門の分類
		 */
		public Default(String name, String code) {
			this.name = name;
			this.code = code;
		}

		@Override
		public String name() {
			return name;
		}

		@Override
		public String code() {
			return code;
		}

		/**
		 * 部門の名前を設定します。
		 *
		 *
		 * @param name 部門の名前
		 */
		public final void setName(String name) {
			this.name = name;
		}

		/**
		 * 部門の分類を設定します。
		 *
		 *
		 * @param code 部門の分類
		 */
		public final void setCode(String code) {
			this.code = code;
		}

		/**
		 * 指定された周波数帯を受容します。
		 *
		 *
		 * @param bands 周波数帯
		 */
		public final void setBands(Band...bands) {
			this.bands = List.of(bands);
		}

		/**
		 * 指定された通信方式を受容します。
		 *
		 *
		 * @param modes 通信方式
		 */
		public final void setModes(Mode...modes) {
			this.modes = List.of(modes);
		}

		/**
		 * 指定された時間帯を受容します。
		 *
		 *
		 * @param hours 時間帯
		 */
		public final void setHours(int...hours) {
			this.hours = new ArrayList<Integer>();
			for(int hr: hours) this.hours.add(hr);
		}

		/**
		 * 指定された時間帯を設定します。
		 *
		 *
		 * @param zoneId 時間帯
		 */
		public final void setZoneId(ZoneId zoneId) {
			this.zoneId = zoneId;
		}

		/**
		 * 指定された地域を受容します。
		 *
		 *
		 * @param codes 地域
		 */
		public final void set(LocalCityBase codes) {
			this.codes = codes;
		}

		@Override
		public final List<LocalCityItem> getCityList() {
			return codes == null? List.of(): codes.toList();
		}

		@Override
		public Message verify(Item item) {
			final var band = (Band) item.getBoth(Qxsl.BAND);
			final var mode = (Mode) item.getBoth(Qxsl.MODE);
			final var code = (Code) item.getRcvd(Qxsl.CODE);
			final var time = (Time) item.getBoth(Qxsl.TIME);
			final var hour = time.atZone(zoneId).value();
			if(!verifyBand(band)) return new Failure(item, band);
			if(!verifyMode(mode)) return new Failure(item, mode);
			if(!verifyTime(hour)) return new Failure(item, time);
			if(!verifyCode(code)) return new Failure(item, code);
			return new Success(item, points(item));
		}

		/**
		 * 指定された周波数帯が規約に準拠するか確認します。
		 *
		 *
		 * @param band 周波数帯
		 *
		 * @return 準拠する場合は真
		 */
		public boolean verifyBand(Band band) {
			return bands == null || bands.contains(band);
		}

		/**
		 * 指定された通信方式が規約に準拠するか確認します。
		 *
		 *
		 * @param mode 通信方式
		 *
		 * @return 準拠する場合は真
		 */
		public boolean verifyMode(Mode mode) {
			return modes == null || modes.contains(mode);
		}

		/**
		 * 指定された交信時刻が規約に準拠するか確認します。
		 *
		 *
		 * @param time 交信時刻
		 *
		 * @return 準拠する場合は真
		 */
		public boolean verifyTime(ZonedDateTime time) {
			return hours == null || hours.contains(time.getHour());
		}

		/**
		 * 指定されたナンバーが規約に準拠するか確認します。
		 *
		 *
		 * @param code ナンバー
		 *
		 * @return 準拠する場合は真
		 */
		public boolean verifyCode(Code code) {
			return codes == null || codes.containsCode(cityOf(code));
		}

		/**
		 * 指定されたナンバーから市区町村番号を抽出します。
		 *
		 *
		 * @param code ナンバー
		 *
		 * @return 市区町村番号
		 */
		public String cityOf(Code code) {
			return code.value().replaceFirst("^599?", "");
		}

		/**
		 * 指定された交信の得点を計算します。
		 *
		 *
		 * @param item 交信
		 *
		 * @return 得点
		 */
		public int points(Item item) {
			return 1;
		}

		/**
		 * 周波数毎の呼出符号を数えます。
		 *
		 *
		 * @param item 検査対象の交信記録
		 *
		 * @return 周波数と呼出符号の組
		 */
		@Override
		public Element unique(Item item) {
			final var call = (Call) item.getBoth(Qxsl.CALL);
			final var band = (Band) item.getBoth(Qxsl.BAND);
			return new Element(call, band);
		}

		/**
		 * 周波数毎のナンバーを数えます。
		 *
		 *
		 * @param item 検査対象の交信記録
		 *
		 * @return 周波数とナンバーの組
		 */
		@Override
		public Element entity(Item item) {
			final var band = (Band) item.getBoth(Qxsl.BAND);
			final var code = (Code) item.getRcvd(Qxsl.CODE);
			return new Element(List.of(band, cityOf(code)));
		}

		/**
		 * 交信の点数とマルチの個数の積を返します。
		 *
		 *
		 * @param items 集計結果
		 *
		 * @return 総得点
		 */
		@Override
		public int result(Summary items) {
			final int score = items.score();
			final var mults = items.keys(0);
			return score > 0? score * mults.size(): 0;
		}
	}
}
