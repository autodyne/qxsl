/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.table.secret;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Random;
import org.junit.Test;
import qxsl.field.*;
import qxsl.model.Item;
import qxsl.table.Tables;
import static org.apache.commons.lang3.RandomStringUtils.*;
import static org.hamcrest.CoreMatchers.*;
import static org.junit.Assert.assertThat;

/**
 * {@see QxmlFormat}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/02/26
 *
 */
public final class QxmlFormatTest extends junit.framework.TestCase {
	private final Tables tables = new Tables();
	private final Random random = new Random();
	@Test
	public void testDecode() throws Exception {
		final QxmlFormat format = new QxmlFormat();
		for(int numItems = 0; numItems <= 100; numItems++) {
			final ArrayList<Item> items = new ArrayList<>();
			for(int row = 0; row < numItems; row++) {
				final Item item = new Item();
				item.set(new Time());
				item.set(new Band(random.nextInt(10_000_000)));
				item.set(new Call(randomAlphanumeric(1, 20)));
				item.set(new Name(randomAlphanumeric(1, 10)));
				item.set(new Note(randomAlphanumeric(1, 10)));
				item.set(new Mode(randomAlphanumeric(1, 10)));
				item.getRcvd().set(new RSTQ(random.nextInt(600)));
				item.getRcvd().set(new Code(randomAlphanumeric(1, 10)));
				item.getRcvd().set(new City(randomAlphanumeric(1, 10)));
				item.getRcvd().set(new Watt(randomAlphanumeric(1, 10)));
				item.getSent().set(new RSTQ(random.nextInt(600)));
				item.getSent().set(new Code(randomAlphanumeric(1, 10)));
				item.getSent().set(new City(randomAlphanumeric(1, 10)));
				item.getSent().set(new Watt(randomAlphanumeric(1, 10)));
				items.add(item);
			}
			final ByteArrayOutputStream os = new ByteArrayOutputStream();
			format.encode(os, items);
			final byte[] b = os.toByteArray();
			assertThat(format.decode(new ByteArrayInputStream(b)), is(items));
			assertThat(tables.decode(new ByteArrayInputStream(b)), is(items));
		}
	}
}
