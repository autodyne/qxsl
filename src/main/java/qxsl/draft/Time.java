/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.draft;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;

import qxsl.value.Tuple;

import static java.time.ZoneId.systemDefault;
import static java.time.temporal.ChronoUnit.MINUTES;

/**
 * 交信の現地時刻を表す属性の実装です。
 *
 *
 * @author 無線部開発班
 *
 * @since 2013/06/08
 */
public final class Time extends Qxsl<ZonedDateTime> {
	/**
	 * 交信日時を指定して属性を構築します。
	 *
	 *
	 * @param time 交信日時
	 */
	public Time(ZonedDateTime time) {
		super(TIME, time);
	}

	/**
	 * 交信日時を指定して属性を構築します。
	 *
	 *
	 * @param time 交信日時
	 */
	public Time(Instant time) {
		this(time.atZone(systemDefault()));
	}

	/**
	 * 交信日時を指定して属性を構築します。
	 *
	 *
	 * @param time 交信日時
	 */
	public Time(LocalDateTime time) {
		this(time.atZone(systemDefault()));
	}

	/**
	 * 交信記録の時刻を抽出します。
	 *
	 *
	 * @param tuple 交信記録
	 *
	 * @return 時刻の属性
	 *
	 * @since 2020/10/28
	 */
	public static final Time from(Tuple tuple) {
		return (Time) tuple.get(Qxsl.TIME);
	}

	/**
	 * 現在時刻で属性を構築します。
	 *
	 *
	 * @return 現在時刻の属性
	 */
	public static final Time now() {
		return new Time(ZonedDateTime.now());
	}

	/**
	 * 指定された年で複製します。
	 *
	 *
	 * @param year 適用される年
	 *
	 * @return 年を変更した複製
	 *
	 * @since 2020/10/28
	 */
	public final Time ofYear(int year) {
		return new Time(value.withYear(year));
	}

	/**
	 * 分までの精度で複製します。
	 *
	 *
	 * @return 秒以下を除外した複製
	 *
	 * @since 2020/10/28
	 */
	public final Time copyDropSecond() {
		return new Time(value.truncatedTo(MINUTES));
	}

	/**
	 * 現地時間の時刻を返します。
	 *
	 *
	 * @return 現地時刻
	 */
	public final ZonedDateTime local() {
		return value.withZoneSameInstant(systemDefault());
	}

	/**
	 * 指定されたオブジェクトと等値であるか確認します。
	 *
	 *
	 * @param obj 比較するオブジェクト
	 *
	 * @return この属性と等しい場合true
	 */
	@Override
	public final boolean equals(Object obj) {
		if(!Time.class.isInstance(obj)) return false;
		else return value.isEqual(((Time) obj).value);
	}
}
