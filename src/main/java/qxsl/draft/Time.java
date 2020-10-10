/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.draft;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;

import qxsl.value.Field;

import static java.time.temporal.ChronoUnit.MINUTES;

/**
 * 交信の日時を表現する{@link Field}実装クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class Time extends Qxsl<ZonedDateTime> {
	private final ZonedDateTime time;

	/**
	 * 現在時刻で属性を構築します。
	 */
	 public Time() {
		this(ZonedDateTime.now());
	 }

	/**
	 * 交信日時を指定して属性を構築します。
	 *
	 *
	 * @param time 交信日時
	 */
	public Time(Instant time) {
		this(time.atZone(ZoneId.systemDefault()));
	}

	/**
	 * 交信日時を指定して属性を構築します。
	 *
	 *
	 * @param time 交信日時
	 */
	public Time(LocalDateTime time) {
		this(time.atZone(ZoneId.systemDefault()));
	}

	/**
	 * 交信日時を指定して属性を構築します。
	 *
	 *
	 * @param time 交信日時
	 */
	public Time(ZonedDateTime time) {
		super(TIME);
		this.time = time;
	}

	@Override
	public final ZonedDateTime value() {
		return time;
	}

	/**
	 * システムのローカルな時刻を返します。
	 *
	 *
	 * @return ローカルな時刻
	 */
	public final ZonedDateTime local() {
		return time.withZoneSameInstant(ZoneId.systemDefault());
	}

	/**
	 * 指定されたオブジェクトと等値であるか確認します。
	 * 比較の対象が分まで同じ時刻の場合に真を返します。
	 *
	 *
	 * @param obj 比較するオブジェクト
	 *
	 * @return この属性と等しい場合に真
	 */
	@Override
	public final boolean equals(Object obj) {
		if(obj instanceof Time) {
			final var comp = ((Time) obj).time;
			return comp.until(time, MINUTES) == 0;
		} else return false;
	}
}
