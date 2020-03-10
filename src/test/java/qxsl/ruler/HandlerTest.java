/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.ruler;

import java.util.LinkedList;
import java.util.List;

import qxsl.model.Item;
import qxsl.table.TableFormats;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import static qxsl.extra.field.Qxsl.*;
import static qxsl.ruler.Handler.FORMAT;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Handler}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2020/02/27
 *
 */
public final class HandlerTest extends test.RandTest {
	@ParameterizedTest
	@MethodSource("loadItems")
	public void testApply(Item iJarl, Item iAdis) {
		final var rJarl = iJarl.getRcvd();
		final var rAdis = iAdis.getRcvd();
		assertThat(iJarl.get(TIME)).isEqualTo(iAdis.get(TIME));
		assertThat(iJarl.get(CALL)).isEqualTo(iAdis.get(CALL));
		assertThat(iJarl.get(BAND)).isEqualTo(iAdis.get(BAND));
		assertThat(iJarl.get(MODE)).isEqualTo(iAdis.get(MODE));
		assertThat(rJarl.get(RSTQ)).isEqualTo(rAdis.get(RSTQ));
		assertThat(rJarl.get(CODE)).isEqualTo(rAdis.get(CODE));
	}

	/**
	 * 指定された書式の交信記録の例をクラスパスから読み出します。
	 *
	 * @param fmt 書式
	 * @return 交信記録
	 *
	 * @throws Exception 読み込み時の例外
	 */
	private static List<Item> load(String fmt) throws Exception {
		final var path = Handler.class.getResource(fmt);
		try(final var strm = path.openStream()) {
			return new TableFormats().decode(strm);
		}
	}

	/**
	 * 同じ内容の交信記録の書式の対をクラスパスから読み出します。
	 *
	 *
	 * @return 交信記録の対のリスト
	 */
	private static List<Arguments> loadItems() throws Exception {
		final var form = new RuleKit().handler(FORMAT);
		final var jarl = form.handle(load("allja1.jarl"));
		final var adis = form.handle(load("allja1.adis"));
		final var list = new LinkedList<Arguments>();
		assertThat(form).hasToString(form.getName());
		for(int i = 0; i < jarl.size(); i++) {
			list.add(Arguments.of(jarl.get(i), adis.get(i)));
		}
		return list;
	}
}
