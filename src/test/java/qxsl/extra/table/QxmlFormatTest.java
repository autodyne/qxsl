/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.table;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import qxsl.extra.field.*;
import qxsl.model.Item;
import qxsl.table.TableFormats;

import java.util.ArrayList;
import java.util.stream.IntStream;

import static qxsl.junit.RandomNumberParameterExtension.randInt;
import static qxsl.junit.RandomStringParameterExtension.alnum;

/**
 * {@link QxmlFormat}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 */
public final class QxmlFormatTest extends org.assertj.core.api.Assertions {
	private final TableFormats tables = new TableFormats();

	public static IntStream testMethodSource() {
		return IntStream.range(0, 100);
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testDecode(int numItems) throws Exception {
		final QxmlFormat format = new QxmlFormat();
		final ArrayList<Item> items = new ArrayList<>();
		for(int row = 0; row < numItems; row++) {
			final Item item = new Item();
			item.set(new Time());
			item.set(new Band(randInt(10_000_000)));
			item.set(new Call(alnum(10)));
			item.set(new Name(alnum(10)));
			item.set(new Note(alnum(10)));
			item.set(new Mode(alnum(10)));
			item.getRcvd().set(new RSTQ(randInt(600)));
			item.getRcvd().set(new Code(alnum(10)));
			item.getRcvd().set(new Watt(alnum(10)));
			item.getSent().set(new RSTQ(randInt(600)));
			item.getSent().set(new Code(alnum(10)));
			item.getSent().set(new Watt(alnum(10)));
			items.add(item);
		}
		assertThat(tables.decode(format.encode(items))).isEqualTo(items);
	}
}
