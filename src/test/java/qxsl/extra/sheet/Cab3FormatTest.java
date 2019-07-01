/*****************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * Language: Java Standard Edition 8
 *****************************************************************************
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics http://pafelog.net
*****************************************************************************/
package qxsl.extra.sheet;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import org.junit.jupiter.api.Test;

import qxsl.extra.field.*;
import qxsl.model.Item;
import qxsl.sheet.SheetFormats;
import qxsl.table.TableFormats;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link Cab3Format}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2019/05/03
 *
 */
public final class Cab3FormatTest extends test.RandTest {
	private final Cab3Format format = new Cab3Format();
	private final SheetFormats sheets = new SheetFormats();
	private final TableFormats tables = new TableFormats();
	private final ArrayList<Band> bands = new ArrayList<>();
	public Cab3FormatTest() {
		bands.add(new Band( 3_500));
		bands.add(new Band( 7_000));
		bands.add(new Band(14_000));
		bands.add(new Band(21_000));
		bands.add(new Band(28_000));
		bands.add(new Band(50_000));
	}
	@Test
	public void testDecode() throws java.io.IOException {
		final ArrayList<Item> items = new ArrayList<>();
		for(int row = 0; row <= randInt(50); row++) {
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
		ByteArrayOutputStream os1 = new ByteArrayOutputStream();
		ByteArrayOutputStream os2 = new ByteArrayOutputStream();
		tables.getFormat("cqww").encode(os1, items);
		String table = os1.toString("UTF-8").trim();
		Map<String, String> kvals = new HashMap<>();
		kvals.put("CONTEST", "JIDX-CW");
		kvals.put("CALLSIGN", "JA1ZLO");
		kvals.put("QSO", table);
		format.encode(os2, kvals);
		final byte[] b = os2.toByteArray();
		assertThat(format.decode(new ByteArrayInputStream(b))).isEqualTo(kvals);
		assertThat(sheets.unseal(new ByteArrayInputStream(b))).isEqualTo(table);
	}
}
