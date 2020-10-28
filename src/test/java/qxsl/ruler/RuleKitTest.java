/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.ruler;

import java.util.ArrayList;
import java.util.List;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import qxsl.draft.Mode;
import qxsl.draft.Name;
import qxsl.draft.Time;
import qxsl.model.Item;
import qxsl.table.TableFactory;
import qxsl.table.TableManager;

import gaas.utils.AssetUtils;

/**
 * {@link RuleKit}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 */
public final class RuleKitTest extends Assertions {
	private static final TableManager fmts = new TableManager();
	private static final AssetUtils asset = new AssetUtils(RuleKitTest.class);
	private static final Contest rules = RuleKit.loadAsContest("allja1.lisp");

	@Test
	public void testForName() {
		assertThat(RuleKit.forName("elva").name()).isEqualTo("elva");
		assertThat(RuleKit.forName("ruby").name()).isEqualTo("ruby");
	}

	@Test
	public void testForFile() {
		assertThat(RuleKit.forFile("q.lisp").name()).isEqualTo("elva");
		assertThat(RuleKit.forFile("xsl.rb").name()).isEqualTo("ruby");
	}

	/**
	 * コンテストの部門の名前と正しい得点を格納します。
	 *
	 *
	 * @since 2020/02/23
	 */
	private static final class Constraint {
		public final String label;
		public final int score;
		public final int total;
		public final String[] forms;

		/**
		 * 部門の名前と素点と総得点を指定します。
		 *
		 * @param vals 名前と素点と総得点の配列
		 */
		public Constraint(String...vals) {
			this.label = vals[0];
			this.score = Integer.parseInt(vals[1]);
			this.total = Integer.parseInt(vals[2]);
			this.forms = vals[3].split(":");
		}

		/**
		 * テスト項目として文字列表現を返します。
		 *
		 *
		 * @return 文字列表現
		 */
		@Override
		public final String toString() {
			return label;
		}
	}

	/**
	 * 得点を計算する部門と正解をクラスパスから読み出します。
	 *
	 *
	 * @return 部門と正解のリスト
	 */
	private static List<Arguments> lines() {
		final var list = new ArrayList<Arguments>();
		final var data = asset.lines("allja1.test");
		for(final var ln: data.toArray(String[]::new)) {
			final var v = new Constraint(ln.split(", +", 4));
			for(var f: v.forms) list.add(Arguments.of(v, f));
		}
		return list;
	}

	/**
	 * 書式変換を試みる交信記録とその書式のリストを返します。
	 *
	 *
	 * @return 交信記録と書式のリスト
	 */
	private static List<Arguments> items() {
		final var list = new ArrayList<Arguments>();
		for(final var item: asset.items("allja1.qxml")) {
			for(var f: fmts) list.add(Arguments.of(item, f));
		}
		return list;
	}

	@ParameterizedTest
	@MethodSource("lines")
	public void testFormat(Constraint line, String fmt) {
		final var sect = rules.getSection(line.label);
		final var path = "allja1.".concat(fmt);
		final var sums = sect.summarize(asset.items(path));
		assertThat(sums.score()).isEqualTo(line.score);
		assertThat(sums.total()).isEqualTo(line.total);
	}

	@ParameterizedTest
	@MethodSource("items")
	public void testFormat(Item item, TableFactory fmt) throws Exception {
		final var seq1 = fmt.encode(rules.transform(item, fmt.getName()));
		final var seq2 = rules.normalize(fmt.decode(seq1), fmt.getName());
		final var back = seq2.get(0);
		back.set(Name.from(item));
		if(Mode.from(back).isRTTY()) back.set(Mode.from(item));
		item.set(Time.from(item).ofYear(2020).copyDropSecond());
		back.set(Time.from(back).ofYear(2020).copyDropSecond());
		assertThat(item).isEqualTo(back);
	}
}
