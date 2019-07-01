/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.ruler;

import java.io.*;
import java.net.URL;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import qxsl.model.Item;
import qxsl.table.TableFormats;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link RuleKit}クラスをALLJA1コンテストの規約でテストするクラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/26
 *
 */
public final class RuleKitTest extends test.RandTest {
	/**
	 * ALLJA1コンテストの各部門の名前と得点の正解を返します。
	 *
	 * 
	 * @return 部門名と得点と乗数のコンテナ
	 * @throws IOException 通常は発生しない
	 */
	private Map<String, int[]> loadScores() throws IOException {
		final HashMap<String, int[]> scores = new HashMap<>();
		final URL path = RuleKit.class.getResource("allja1.test");
		final Reader r = new InputStreamReader(path.openStream());
		try (BufferedReader br = new BufferedReader(r)) {
			br.lines().forEach(line -> {
				final String[] vals = line.split(",", 3);
				final int calls = Integer.parseInt(vals[1]);
				final int mults = Integer.parseInt(vals[2]);
				scores.put(vals[0].trim(), new int[]{calls, mults});
			});
		}
		return scores;
	}
	/**
	 * ALLJA1コンテストの各書式による交信記録のURLを返します。
	 *
	 * 
	 * @return URLのリスト
	 */
	public static List<URL> testMethodSource() {
		return Arrays.asList(
			RuleKit.class.getResource("allja1.adxs"),
			RuleKit.class.getResource("allja1.cqww"),
			RuleKit.class.getResource("allja1.ctxt"),
			RuleKit.class.getResource("allja1.hl76"),
			RuleKit.class.getResource("allja1.jarl"),
			RuleKit.class.getResource("allja1.rtcl"),
			RuleKit.class.getResource("allja1.zall"),
			RuleKit.class.getResource("allja1.zdos"),
			RuleKit.class.getResource("allja1.cbin"),
			RuleKit.class.getResource("allja1.zbin")
		);
	}
	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void test(URL path) throws Exception {
		List<Item> list = (new TableFormats()).decode(path);
		final Map<String, int[]> scores = this.loadScores();
		final Contest test = Contest.defined("allja1.lisp");
		for(String sect: scores.keySet()) {
			Summary sum = test.getSection(sect).summarize(list);
			assertThat(sum.score()).isEqualTo(scores.get(sect)[0]);
			assertThat(sum.mults()).isEqualTo(scores.get(sect)[1]);
		}
	}
}
