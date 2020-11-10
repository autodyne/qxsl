/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.table;

import java.util.ArrayList;
import java.util.stream.IntStream;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.xml.sax.SAXException;

import qxsl.draft.*;
import qxsl.model.Item;
import qxsl.table.TableManager;

import static qxsl.junit.RandomNumberParameterExtension.randInt;
import static qxsl.junit.RandomStringParameterExtension.alnum;

/**
 * {@link QxmlFactory}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/02/26
 */
public final class QxmlFactoryTest extends Assertions {
	private final TableManager tables = new TableManager();

	@ParameterizedTest
	@MethodSource("source")
	public void testDecode(int numItems) throws SAXException {
		final var items = new ArrayList<Item>();
		for (int row = 0; row < numItems; row++) {
			final var item = new Item();
			item.set(Time.now().copyDropSecond());
			item.set(new Band(randInt(10_000_000)));
			item.set(new Call(alnum(10)));
			item.set(new Name(alnum(10)));
			item.set(new Note(alnum(10)));
			item.set(new Mode(alnum(10)));
			item.set(new Mul1(alnum(10)));
			item.set(new Mul2(alnum(10)));
			item.getRcvd().set(new RSTQ(randInt(600)));
			item.getRcvd().set(new Code(alnum(10)));
			item.getRcvd().set(new Watt(alnum(10)));
			item.getSent().set(new RSTQ(randInt(600)));
			item.getSent().set(new Code(alnum(10)));
			item.getSent().set(new Watt(alnum(10)));
			items.add(item);
		}
		final var format = new QxmlFactory();
		assertThat(format.decode(format.encode(items))).isEqualTo(items);
		assertThat(tables.decode(format.encode(items))).isEqualTo(items);
	}

	public static final IntStream source() {
		return IntStream.range(0, 100);
	}
}
