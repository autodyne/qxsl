/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.field;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Serializable;
import java.io.UncheckedIOException;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import javax.xml.namespace.QName;

import qxsl.field.FieldFactory;
import qxsl.model.Field;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.stream.Collectors.toList;

/**
 * 交信の相手局の地域を表現する{@link Field}実装クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2014/04/20
 */
public final class City extends Qxsl<String> {
	private final String base;
	private final String code;
	private final List<String> path;

	/**
	 * 地域ベースと地域データを指定して地域を構築します。
	 *
	 *
	 * @param base 地域ベースの名前
	 * @param data 地域名と地域番号の配列
	 */
	private City(String base, String...data) {
		super(CITY);
		final int last = data.length - 1;
		this.base = base;
		this.code = data[last];
		this.path = List.of(data).subList(0, last);
	}

	@Override
	public String value() {
		return base.concat(":").concat(code);
	}

	/**
	 * この地域を識別する地域番号を返します。
	 *
	 * @return 地域番号
	 */
	public String getCode() {
		return code;
	}

	/**
	 * この地域を識別する名称を行政区画のリストで返します。
	 * 例えば東京都目黒区の場合、東京都と目黒区を返します。
	 *
	 * @return 地域名のリスト
	 */
	public List<String> getFullPath() {
		return path.stream().distinct().collect(toList());
	}

	/**
	 * この地域を識別する完全な名称を連結文字列で返します。
	 * 例えば東京都目黒区の場合、"東京都目黒区"を返します。
	 *
	 * @return 地域名
	 */
	public String getFullName() {
		return String.join("", getFullPath());
	}

	/**
	 * 地域ベースと地域番号を指定して地域を返します。
	 *
	 *
	 * @param code 地域ベースの名前:地域番号
	 *
	 * @return 地域 不明な地域番号に対してはnull
	 *
	 * @throws UncheckedIOException 地域ベースが未知の場合
	 */
	public static City forCode(String code) {
		return forCode(code.split(":")[0], code.split(":")[1]);
	}

	/**
	 * 地域ベースと地域番号を指定して地域を返します。
	 *
	 *
	 * @param base 地域ベースの名前
	 * @param code 地域番号
	 *
	 * @return 地域 不明な地域番号に対してはnull
	 *
	 * @throws UncheckedIOException 地域ベースが未知の場合
	 */
	public static City forCode(String base, String code) {
		for(var city: DataBase.forName(base).cities.values()) {
			if(city.code.equals(code)) return city;
		}
		return null;
	}

	/**
	 * ライブラリに内臓された地域ベースが含む全ての地域を返します。
	 *
	 *
	 * @param base 地域ベースの名前
	 *
	 * @return 全ての利用可能な地域
	 *
	 * @throws UncheckedIOException 地域ベースが未知の場合
	 */
	public static final List<City> all(String base) {
		final var cities = DataBase.forName(base).cities;
		return cities.values().stream().collect(toList());
	}

	/**
	 * ライブラリが内蔵する地域ベースを表現します。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2017/02/28
	 */
	private static final class DataBase implements Serializable {
		private static final Map<String,DataBase> bases = new HashMap<>();
		private final Map<String, City> cities;

		/**
		 * 指定された名前の地域ベースをリソースから読み込みます。
		 *
		 *
		 * @param name 地域ベースの名前
		 *
		 * @throws UncheckedIOException 地域ベースが未知の場合
		 */
		private DataBase(String name) throws UncheckedIOException {
			this.cities = new LinkedHashMap<>();
			final var f = name.concat(".dat");
			final var is = City.class.getResourceAsStream(f);
			final var isr = new InputStreamReader(is, UTF_8);
			try(final var br = new BufferedReader(isr)) {
				var stream = br.lines().map(v -> v.split(" +"));
				var cities = stream.map(v -> new City(name, v));
				cities.forEach(c -> this.cities.put(c.code, c));
			} catch (IOException ex) {
				throw new UncheckedIOException(ex);
			}
		}

		/**
		 * 指定された名前の地域ベースを返します。
		 *
		 *
		 * @param base 地域ベースの名前
		 *
		 * @return 地域ベース
		 *
		 * @throws UncheckedIOException 地域ベースが未知の場合
		 */
		public static final DataBase forName(String base) {
			return bases.computeIfAbsent(base, DataBase::new);
		}
	}

	/**
	 * {@link City}を生成する書式です。
	 *
	 *
	 * @author 無線部開発班
	 *
	 * @since 2014/04/20
	 */
	public static final class Factory implements FieldFactory {
		@Override
		public QName target() {
			return CITY;
		}

		@Override
		public City decode(String value) {
			return City.forCode(value);
		}

		@Override
		public String encode(Field field) {
			return field.value().toString();
		}
	}
}
