/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package qxsl.extra.table;

import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.stream.IntStream;
import javax.xml.namespace.QName;

import qxsl.field.FieldFormats.Any;
import qxsl.model.Item;
import qxsl.table.TableFormats;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import static test.RandTest.*;

/**
 * {@link AdisFormat}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/07/09
 *
 */
public final class AdisFormatTest extends org.assertj.core.api.Assertions {
	private final TableFormats tables = new TableFormats();
	public static IntStream testMethodSource() {
		return IntStream.range(0, 100);
	}
	@ParameterizedTest
	@MethodSource("testMethodSource")
	public void testDecode(int numItems) throws Exception {
		final String ADIF = "adif.org";
		final AdisFormat format = new AdisFormat();
		final ArrayList<Item> items = new ArrayList<>();
		for(int row = 0; row < numItems; row++) {
			final Item item = new Item();
			item.add(new Any(new QName(ADIF, "CALL"), alnum(10)));
			item.add(new Any(new QName(ADIF, "BAND"), alnum(10)));
			item.add(new Any(new QName(ADIF, "MODE"), alnum(10)));
			items.add(item);
		}
		final ByteArrayOutputStream os = new ByteArrayOutputStream();
		format.encoder(os).encode(items);
		assertThat(tables.decode(os.toByteArray())).isEqualTo(items);
	}
}
