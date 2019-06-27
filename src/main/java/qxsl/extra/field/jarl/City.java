/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.field.jarl;

import java.io.*;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;
import javax.xml.namespace.QName;
import qxsl.model.Field;
import qxsl.table.FieldFormat;

/**
 * 日本アマチュア無線連盟の市区町村番号を表現する{@link Field}実装クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2014/04/20
 * 
 */
public final class City extends Jarl<String> {
	private static final DataBase db = new DataBase("city");
	private final String code;

	/**
	 * 市区町村番号を指定して{@link City}を構築します。
	 * 
	 * @param code 市区町村番号
	 */
	public City(String code) {
		super(CITY);
		this.code = code;
	}

	@Override
	public String value() {
		return code;
	}

	/**
	 * 指定された市区町村名に対して{@link City}を返します。
	 *
	 * @param name 市区町村名
	 * @return 市区町村 見つからない場合null
	 */
	public static final City forName(String name) {
		try {
			return new City(db.reverse.get(name).code);
		} catch(NullPointerException ex) {
			return null;
		}
	}

	/**
	 * この{@link City}が所属する市区町村の名前を返します。
	 *
	 * @return 市区町村名 見つからない場合null
	 */
	public String getCityName() {
		try {
			return db.forward.get(code).name;
		} catch(NullPointerException ex) {
			return null;
		}
	}

	/**
	 * この{@link City}が所属する都道府県の名前を返します。
	 *
	 * @return 都道府県名 見つからない場合null
	 */
	public String getPrefName() {
		try {
			return db.forward.get(code).pref;
		} catch(NullPointerException ex) {
			return null;
		}
	}

	/**
	 * ライブラリが内蔵する全ての市区町村番号を返します。
	 *
	 * @return 全ての利用可能な市区町村番号
	 */
	public static final List<String> getAvailableCodes() {
		return Collections.unmodifiableList(db.codes);
	}
	
	/**
	 * ライブラリが内蔵する市区町村データベースを表現します。
	 * 
	 *
	 * @author Journal of Hamradio Informatics
	 *
	 * @since 2017/02/28
	 */
	private static final class DataBase {
		public final Map<String, CityEntry> forward;
		public final Map<String, CityEntry> reverse;
		public final List<String> codes;

		/**
		 * 指定されたファイルからデータベースを構築します。
		 *
		 * @param name データベースの名前
		 */
		public DataBase(final String name) {
			URL url = getClass().getResource(name);
			forward = new HashMap<>();
			reverse = new HashMap<>();
			codes = new ArrayList<>();
			try(InputStream is = url.openStream()) {
				load(is);
			} catch(IOException ex) {
				assert true: "failed to read " + url;
			}
		}

		/**
		 * 指定された入力からデータを読み込みます。
		 *
		 * @param is 入力となるストリーム
		 */
		private void load(InputStream is) throws IOException {
			final Reader r = new InputStreamReader(is, "UTF-8");
			Stream<String> data = new BufferedReader(r).lines();
			for(String line: (Iterable<String>) data::iterator) {
				String[] vals = line.split(" +", 3);
				CityEntry ent = new CityEntry(vals);
				forward.put(ent.code, ent);
				reverse.put(ent.name, ent);
				codes.add(ent.code);
			}
		}

		/**
		 * 市区町村データベースのエントリを表現します。
		 * 
		 * @since 2015/08/17
		 */
		private static final class CityEntry {
			public final String code;
			public final String name;
			public final String pref;
			/**
			 * 番号と市区町村名と都道府県名を指定します。
			 *
			 * @param vals 値の配列
			 */
			public CityEntry(String[] vals) {
				this.code = vals[2];
				this.pref = vals[0];
				if(vals[1].equals(pref)) {
					this.name = vals[1];
				} else {
					this.name = vals[0] + vals[1];
				}
			}
		}
	}

	/**
	 * {@link City}を生成するフォーマットです。
	 * 
	 * 
	 * @author Journal of Hamradio Informatics
	 * 
	 * @since 2014/04/20
	 *
	 */
	public static final class Format implements FieldFormat {
		@Override
		public QName name() {
			return CITY;
		}
	
		@Override
		public City decode(String value) {
			return new City(value);
		}
	
		@Override
		public String encode(Field field) {
			return field.value().toString();
		}
	}
}
