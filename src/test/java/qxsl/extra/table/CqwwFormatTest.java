/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*****************************************************************************/
package qxsl.extra.table;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.stream.IntStream;

import qxsl.extra.field.*;
import qxsl.model.Item;
import qxsl.table.TableFormats;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link CqwwFormat}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/04
 *
 */
public final class CqwwFormatTest extends test.RandTest {
	private final CqwwFormat format = new CqwwFormat();
	private final TableFormats tables = new TableFormats();
	private final ArrayList<Band> bands = new ArrayList<>();
	public CqwwFormatTest() {
		bands.add(new Band( 1_800));
		bands.add(new Band( 3_500));
		bands.add(new Band( 7_000));
		bands.add(new Band(14_000));
		bands.add(new Band(21_000));
		bands.add(new Band(28_000));
		bands.add(new Band(50_000));
	}
	public static IntStream testMethodSource() {
		return IntStream.range(0, 100);
	}
	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testDecode(int numItems) throws IOException {
		final ArrayList<Item> items = new ArrayList<>();
		for(int row = 0; row < numItems; row++) {
			final Item item = new Item();
			item.add(new Time());
			item.add(bands.get(randInt(bands.size())));
			item.add(new Call(alnum(13)));
			item.add(new Mode(alnum(2)));
			item.getRcvd().add(new RSTQ(randInt(600)));
			item.getRcvd().add(new Code(alnum(6)));
			item.getSent().add(new RSTQ(randInt(600)));
			item.getSent().add(new Code(alnum(6)));
			items.add(item);
		}
		final ByteArrayOutputStream os = new ByteArrayOutputStream();
		format.encoder(os).encode(items);
		assertThat(tables.decode(os.toByteArray())).isEqualTo(items);
	}
}
