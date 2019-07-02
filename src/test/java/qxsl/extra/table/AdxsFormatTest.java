/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.table;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.stream.IntStream;
import javax.xml.namespace.QName;

import qxsl.field.FieldFormats.Any;
import qxsl.model.Item;
import qxsl.table.TableFormats;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link AdxsFormat}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/07/02
 *
 */
public final class AdxsFormatTest extends test.RandTest {
	private final TableFormats tables = new TableFormats();
	public static IntStream testMethodSource() {
		return IntStream.range(0, 100);
	}
	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testDecode(int numItems) throws Exception {
		final String ADIF = "adif.org";
		final AdxsFormat format = new AdxsFormat();
		final ArrayList<Item> items = new ArrayList<>();
		for(int row = 0; row < numItems; row++) {
			final Item item = new Item();
			item.add(new Any(new QName(ADIF, "CALL"), alnum(10)));
			item.add(new Any(new QName(ADIF, "BAND"), alnum(10)));
			item.add(new Any(new QName(ADIF, "MODE"), alnum(10)));
			items.add(item);
		}
		final ByteArrayOutputStream os = new ByteArrayOutputStream();
		format.encode(os, items);
		final byte[] b = os.toByteArray();
		assertThat(format.decode(new ByteArrayInputStream(b))).isEqualTo(items);
		assertThat(tables.decode(new ByteArrayInputStream(b))).isEqualTo(items);
	}
}