/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.table;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import qxsl.field.FieldFormats.Any;
import qxsl.model.Item;
import qxsl.table.TableFormats;

import java.io.IOException;
import java.util.ArrayList;
import java.util.stream.IntStream;
import javax.xml.namespace.QName;

import static qxsl.junit.RandomStringParameterExtension.alnum;

/**
 * {@link AdisFormat}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/07/09
 */
public final class AdisFormatTest extends org.assertj.core.api.Assertions {
	private final TableFormats tables = new TableFormats();

	public static IntStream testMethodSource() {
		return IntStream.range(0, 100);
	}

	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testDecode(int numItems) throws IOException {
		final String ADIF = "adif.org";
		final AdisFormat format = new AdisFormat();
		final ArrayList<Item> items = new ArrayList<>();
		for(int row = 0; row < numItems; row++) {
			final Item item = new Item();
			item.set(new Any(new QName(ADIF, "CALL"), alnum(10)));
			item.set(new Any(new QName(ADIF, "BAND"), alnum(10)));
			item.set(new Any(new QName(ADIF, "MODE"), alnum(10)));
			items.add(item);
		}
		assertThat(tables.decode(format.encode(items))).isEqualTo(items);
	}
}
