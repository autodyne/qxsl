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
import java.util.Random;
import org.junit.Test;

import qxsl.extra.field.qxsl.*;
import qxsl.model.Item;
import qxsl.sheet.Sheets;
import qxsl.table.Tables;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * {@link JarlFormat}クラスのテスト用クラスです。
 * 
 * 
 * @author Journal of Hamradio Informatics
 * 
 * @since 2017/03/12
 *
 */
public final class JarlFormatTest extends junit.framework.TestCase {
	private final JarlFormat format = new JarlFormat();
	private final Sheets sheets = new Sheets();
	private final Tables tables = new Tables();
	private final ArrayList<Band> bands = new ArrayList<>();
	private final Random random = new Random();
	public JarlFormatTest() {
		bands.add(new Band(    3_500));
		bands.add(new Band(    7_000));
		bands.add(new Band(   14_000));
		bands.add(new Band(  144_000));
		bands.add(new Band(1_200_000));
		bands.add(new Band(5_600_000));
	}
	@Test
	public void testDecode() throws java.io.IOException {
		final ArrayList<Item> items = new ArrayList<>();
		for(int row = 0; row < random.nextInt(50); row++) {
			final Item item = new Item();
			item.set(new Time());
			item.set(bands.get(random.nextInt(bands.size())));
			item.set(new Call(util.RandText.alnum(13)));
			item.set(new Mode(util.RandText.alnum(5)));
			item.getRcvd().set(new RSTQ(random.nextInt(600)));
			item.getRcvd().set(new Code(util.RandText.alnum(7)));
			item.getSent().set(new RSTQ(random.nextInt(600)));
			item.getSent().set(new Code(util.RandText.alnum(7)));
			items.add(item);
		}
		ByteArrayOutputStream os1 = new ByteArrayOutputStream();
		ByteArrayOutputStream os2 = new ByteArrayOutputStream();
		tables.getFormat("jarl").encode(os1, items);
		String table = os1.toString("UTF-8").trim();
		Map<String, String> kvals = new HashMap<>();
		kvals.put("VERSION", "R2.0");
		kvals.put("SCORE BAND=144MHz", "10,10,10");
		kvals.put("SCORE BAND=430MHz", "20,20,20");
		kvals.put("LOGSHEET", os1.toString("SJIS").trim());
		format.encode(os2, kvals);
		final byte[] b = os2.toByteArray();
		assertThat(format.decode(new ByteArrayInputStream(b))).isEqualTo(kvals);
		assertThat(sheets.unseal(new ByteArrayInputStream(b))).isEqualTo(table);
	}
}
