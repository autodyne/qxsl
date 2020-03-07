/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.extra.table;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.stream.IntStream;

import qxsl.extra.field.*;
import qxsl.model.Item;
import qxsl.table.TableFormats;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link QxmlFormat}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 *
 */
public final class QxmlFormatTest extends test.RandTest {
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
			item.add(new Time());
			item.add(new Band(randInt(10_000_000)));
			item.add(new Call(alnum(10)));
			item.add(new Name(alnum(10)));
			item.add(new Note(alnum(10)));
			item.add(new Mode(alnum(10)));
			item.getRcvd().add(new RSTQ(randInt(600)));
			item.getRcvd().add(new Code(alnum(10)));
			item.getRcvd().add(new Watt(alnum(10)));
			item.getSent().add(new RSTQ(randInt(600)));
			item.getSent().add(new Code(alnum(10)));
			item.getSent().add(new Watt(alnum(10)));
			items.add(item);
		}
		final ByteArrayOutputStream os = new ByteArrayOutputStream();
		format.encoder(os).encode(items);
		assertThat(tables.decode(os.toByteArray())).isEqualTo(items);
	}
}
