/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field;

import java.io.*;
import java.net.URL;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Stream;
import javax.xml.namespace.QName;

import qxsl.field.FieldFormat;
import qxsl.model.Field;

/**
 * 交信の相手局の運用地域を表現する{@link Field}実装クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2014/04/20
 * 
 */
public final class City extends Qxsl<String> {
	private static final Map<String,DataBase> bases = new HashMap<>();
	private final String base;
	private final String code;

	/**
	 * 地域番号を指定して運用地域を構築します。
	 * 
	 * @param base 地域データベースの名前
	 * @param code 地域番号
	 */
	public City(String base, String code) {
		super(CITY);
		this.base = base;
		this.code = code;
	}

	@Override
	public String value() {
		return base.concat(":").concat(code);
	}

	/**
	 * 指定された区分におけるこの運用地域の名前を返します。
	 *
	 * @param level 区分の階層
	 * @return 地域名 見つからない場合null
	 */
	public String getName(int level) {
		try {
			return getDataBase(base).get(code)[level];
		} catch(NullPointerException ex) {
			return null;
		}
	}

	/**
	 * この運用地域が末端区分の要素であるか確認します。
	 * 例えば、日本の市区町村である場合は真を返します。
	 *
	 * @return 末端区分の要素である場合true
	 */
	public boolean isTerminal() {
		try {
			String[] codes = getDataBase(base).get(code);
			final String upper = codes[codes.length-2];
			final String lower = codes[codes.length-1];
			return !upper.equals(lower);
		} catch (NullPointerException ex) {
			return false;
		} catch (IndexOutOfBoundsException ex) {
			return true;
		}
	}

	/**
	 * ライブラリが内蔵する全ての地域番号を返します。
	 *
	 * @param base 地域データベースの名前
	 * @return 全ての利用可能な地域番号
	 */
	public static final Collection<String> getCodes(String base) {
		try {
			return getDataBase(base).keys();
		} catch(NullPointerException ex) {
			return null;
		}
	}

	/**
	 * 指定された名前の地域データベースを返します。
	 *
	 * @param base 地域データベースの名前
	 * @return 地域データベース
	 */
	private static final DataBase getDataBase(String base) {
		return bases.computeIfAbsent(base, DataBase::new);
	}

	/**
	 * ライブラリが内蔵する地域データベースを表現します。
	 * 
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/28
	 */
	private static final class DataBase {
		private final Map<String, String[]> map;

		/**
		 * 指定されたファイルからデータベースを構築します。
		 *
		 * @param name データベースの名前
		 */
		public DataBase(String name) {
			this.map = new LinkedHashMap<>();
			final String file = name.concat(".dat");
			URL path = City.class.getResource(file);
			try(InputStream is = path.openStream()) {
				load(is);
			} catch(IOException ex) {}
		}

		/**
		 * 指定された入力からデータを読み込みます。
		 *
		 * @param is 入力となるストリーム
		 * @throws IOException 特にファイルが存在しない場合
		 */
		private void load(InputStream is) throws IOException {
			final Reader r = new InputStreamReader(is, "UTF-8");
			Stream<String> data = new BufferedReader(r).lines();
			Stream<String[]> dsv = data.map(l -> l.split(" +"));
			dsv.forEach(v -> map.put(v[v.length - 1], v));
		}

		/**
		 * 指定された地域番号に対して地域名の配列を返します。
		 *
		 * @param code 地域番号
		 * @return 地域名の配列
		 */
		public final String[] get(String code) {
			final String[] name = map.get(code);
			return Arrays.copyOfRange(name, 0, name.length - 1);
		}

		/**
		 * このデータベースの地域番号の不変の集合を返します。
		 *
		 * @return 地域番号の集合
		 */
		public final Collection<String> keys() {
			return Collections.unmodifiableCollection(map.keySet());
		}
	}

	/**
	 * {@link City}を生成する書式です。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2014/04/20
	 *
	 */
	public static final class Format implements FieldFormat {
		@Override
		public QName target() {
			return CITY;
		}

		@Override
		public City decode(String value) {
			String[] values = value.split(":", 2);
			return new City(values[0], values[1]);
		}

		@Override
		public String encode(Field field) {
			return field.value().toString();
		}
	}
}
