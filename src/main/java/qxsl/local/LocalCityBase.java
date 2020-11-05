/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.local;

import java.io.UncheckedIOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import gaas.utils.AssetUtils;

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
	 * @param stream 内容
	 */
	private LocalCityBase(Stream<LocalCityItem> stream) {
		this.list = stream.collect(Collectors.toList());
		this.forwardMap = new HashMap<>();
		this.reverseMap = new HashMap<>();
		for(var v: list) forwardMap.put(v.code(), v);
		for(var v: list) reverseMap.put(v.name(), v);
	}

	/**
	 * このデータベースが収録する地域のリストを返します。
	 *
	 *
	 * @return 全ての利用可能な地域
	 */
	public final List<LocalCityItem> toList() {
		return Collections.unmodifiableList(list);
	}

	/**
	 * 指定された識別子に対応する地域を返します。
	 *
	 *
	 * @param code 地域の識別子
	 *
	 * @return 地域
	 */
	public final LocalCityItem getByCode(String code) {
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
	public final LocalCityItem getByName(String name) {
		return this.reverseMap.get(name);
	}

	/**
	 * 指定された内蔵データベースを読み取ります。
	 *
	 *
	 * @param name データベースの名前
	 *
	 * @return データベース
	 *
	 * @throws UncheckedIOException 存在しない場合
	 */
	public static final LocalCityBase load(String name) {
		final var lines = AssetUtils.root().lines(name);
		final var route = lines.map(LocalCityItem::new);
		return new LocalCityBase(route);
	}
}
