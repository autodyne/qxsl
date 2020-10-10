/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.local;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import static java.nio.charset.StandardCharsets.UTF_8;

/**
 * ライブラリが内蔵する都市または地域のデータベースです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/28
 */
public final class LocalCityBase {
	private final Map<String, LocalCityItem> forwardMap;
	private final Map<String, LocalCityItem> reverseMap;
	private final List<LocalCityItem> list;

	/**
	 * 指定された内容を収録するデータベースを構築します。
	 *
	 *
	 * @param list 内容
	 */
	private LocalCityBase(List<LocalCityItem> list) {
		this.list = Collections.unmodifiableList(list);
		this.forwardMap = new HashMap<>();
		this.reverseMap = new HashMap<>();
		for(var v: list) this.forwardMap.put(v.getCode(), v);
		for(var v: list) this.reverseMap.put(v.getName(), v);
	}

	/**
	 * このデータベースが収録する地域のリストを返します。
	 *
	 *
	 * @return 全ての利用可能な地域
	 */
	public final List<LocalCityItem> toList() {
		return this.list;
	}

	/**
	 * 指定された識別子に対応する地域を返します。
	 *
	 *
	 * @param code 地域の識別子
	 *
	 * @return 地域
	 */
	public final LocalCityItem getCityByCode(String code) {
		return this.forwardMap.get(code);
	}

	/**
	 * 指定された自然名に対応する地域を返します。
	 *
	 *
	 * @param name 地域の自然名
	 *
	 * @return 地域
	 */
	public final LocalCityItem getCityByName(String name) {
		return this.reverseMap.get(name);
	}

	/**
	 * 指定された名前のデータベースを読み取って返します。
	 *
	 *
	 * @param name データベースの名前
	 *
	 * @return データベース
	 */
	public static final LocalCityBase load(String name) {
		final var TYPE = LocalCityBase.class;
		final var strm = TYPE.getResourceAsStream(name);
		final var text = new InputStreamReader(strm, UTF_8);
		try(final var b = new BufferedReader(text)) {
			final var data = b.lines().map(LocalCityItem::new);
			final var list = data.collect(Collectors.toList());
			return new LocalCityBase(list);
		} catch (IOException ex) {
			return null;
		}
	}
}
