/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Released under the GNU Lesser General Public License (LGPL) v3 (see LICENSE)
 * Univ. Tokyo Amateur Radio Club Development Task Force (https://nextzlog.dev)
*******************************************************************************/
package qxsl.utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UncheckedIOException;
import java.net.URL;
import java.util.List;
import java.util.Properties;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import qxsl.model.Item;
import qxsl.table.TableManager;

import static java.nio.charset.StandardCharsets.UTF_8;
import static java.util.stream.Collectors.joining;

/**
 * ライブラリが内蔵するリソースを取り出します。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/10/27
 */
public final class AssetUtil {
	private final Class<?> type;

	/**
	 * 指定されたクラスを起点にリソースを検索します。
	 *
	 *
	 * @param type 検索の起点となるクラス
	 */
	public AssetUtil(Class<?> type) {
		this.type = type;
	}

	/**
	 * クラスパスの頂点を起点にリソースを検索します。
	 *
	 *
	 * @return インスタンス
	 */
	public static final AssetUtil root() {
		return new AssetUtil(null);
	}

	/**
	 * 引数の実装クラスを起点にリソースを検索します。
	 *
	 *
	 * @param obj 検索の起点となるオブジェクト
	 *
	 * @return インスタンス
	 */
	public static final AssetUtil from(Object obj) {
		return new AssetUtil(obj.getClass());
	}

	/**
	 * ライブラリを読み込んだクラスローダを返します。
	 *
	 *
	 * @return クラスローダ
	 *
	 * @since 2020/10/27
	 */
	public final ClassLoader getClassLoader() {
		return AssetUtil.class.getClassLoader();
	}

	/**
	 * 指定されたリソースをプロパティに読み取ります。
	 *
	 *
	 * @param name リソースのパス
	 *
	 * @return プロパティ
	 *
	 * @throws UncheckedIOException 読み取りに失敗した場合
	 */
	public final Properties properties(String name) {
		final var path = name.concat(".xml");
		final var prop = new Properties();
		try(final var strm = stream(path)) {
			prop.loadFromXML(strm);
		} catch (IOException ex) {
			throw new UncheckedIOException(ex);
		}
		return prop;
	}

	/**
	 * 指定されたリソースから交信記録を読み取ります。
	 *
	 *
	 * @param path リソースのパス
	 *
	 * @return 交信記録
	 *
	 * @throws UncheckedIOException 読み取りに失敗した場合
	 */
	public final List<Item> items(String path) {
		try(final var stream = stream(path)) {
			final var bytes = stream.readAllBytes();
			return new TableManager().decode(bytes);
		} catch (IOException ex) {
			throw new UncheckedIOException(ex);
		}
	}

	/**
	 * 指定されたリソースを行の列として読み取ります。
	 *
	 *
	 * @param path リソースのパス
	 *
	 * @return 行の列
	 *
	 * @throws UncheckedIOException 読み取りに失敗した場合
	 */
	public final List<String> listLines(String path) {
		return lines(path).collect(Collectors.toList());
	}

	/**
	 * 指定されたリソースを行の列として読み取ります。
	 *
	 *
	 * @param path リソースのパス
	 *
	 * @return 行の列
	 *
	 * @throws UncheckedIOException 読み取りに失敗した場合
	 */
	public final Stream<String> lines(String path) {
		return string(path).lines();
	}

	/**
	 * 指定されたリソースを文字列として読み取ります。
	 *
	 *
	 * @param path リソースのパス
	 *
	 * @return 文字列
	 *
	 * @throws UncheckedIOException 読み取りに失敗した場合
	 */
	public final String string(String path) {
		try(final var br = this.buffer(path)) {
			return br.lines().collect(joining("\n"));
		} catch (IOException ex) {
			throw new UncheckedIOException(ex);
		}
	}

	/**
	 * 指定されたリソースを読み取る準備をします。
	 *
	 *
	 * @param path リソースのパス
	 *
	 * @return リーダ
	 */
	public final BufferedReader buffer(String path) {
		return new BufferedReader(reader(path));
	}

	/**
	 * 指定されたリソースを読み取る準備をします。
	 *
	 *
	 * @param path リソースのパス
	 *
	 * @return リーダ
	 */
	public final InputStreamReader reader(String path) {
		return new InputStreamReader(stream(path), UTF_8);
	}

	/**
	 * 指定されたリソースを読み取る準備をします。
	 *
	 *
	 * @param path リソースのパス
	 *
	 * @return ストリーム
	 */
	public final InputStream stream(String path) {
		if(type != null) return type.getResourceAsStream(path);
		else return getClassLoader().getResourceAsStream(path);
	}

	/**
	 * 指定されたウェブページの文字列を読み取ります。
	 *
	 *
	 * @param url URL
	 * @param chs 文字セット
	 *
	 * @return 文字列
	 *
	 * @throws IOException 読み取りに失敗した場合
	 *
	 * @since 2023/05/03
	 */
	public final String http(URL url, String chs) throws IOException {
		try(var strm = new InputStreamReader(url.openStream(), chs)) {
			return new BufferedReader(strm).lines().collect(joining("\n"));
		}
	}
}
