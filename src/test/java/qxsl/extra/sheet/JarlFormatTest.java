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

import qxsl.extra.field.*;
import qxsl.model.Item;
import qxsl.sheet.SheetFormats;
import qxsl.table.TableFormats;

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
	private final SheetFormats sheets = new SheetFormats();
	private final TableFormats tables = new TableFormats();
	private final ArrayList<Freq> freqs = new ArrayList<>();
	private final Random random = new Random();
	public JarlFormatTest() {
		freqs.add(new Freq(    3_500));
		freqs.add(new Freq(    7_000));
		freqs.add(new Freq(   14_000));
		freqs.add(new Freq(  144_000));
		freqs.add(new Freq(1_200_000));
		freqs.add(new Freq(5_600_000));
	}
	@Test
	public void testDecode() throws java.io.IOException {
		final ArrayList<Item> items = new ArrayList<>();
		for(int row = 0; row < random.nextInt(50); row++) {
			final Item item = new Item();
			item.add(new Time());
			item.add(freqs.get(random.nextInt(freqs.size())));
			item.add(new Call(util.RandText.alnum(13)));
			item.add(new Mode(util.RandText.alnum(5)));
			item.getRcvd().add(new RSTQ(random.nextInt(600)));
			item.getRcvd().add(new Code(util.RandText.alnum(7)));
			item.getSent().add(new RSTQ(random.nextInt(600)));
			item.getSent().add(new Code(util.RandText.alnum(7)));
			items.add(item);
		}
		ByteArrayOutputStream os1 = new ByteArrayOutputStream();
		ByteArrayOutputStream os2 = new ByteArrayOutputStream();
		tables.getFormat("jarl").encode(os1, items);
		String table = os1.toString("UTF-8").trim();
		Map<String, String> kvals = new HashMap<>();
		kvals.put("VERSION", "R2.0");
		kvals.put("SCORE FREQ=144MHz", "10,10,10");
		kvals.put("SCORE FREQ=430MHz", "20,20,20");
		kvals.put("LOGSHEET", os1.toString("SJIS").trim());
		format.encode(os2, kvals);
		final byte[] b = os2.toByteArray();
		assertThat(format.decode(new ByteArrayInputStream(b))).isEqualTo(kvals);
		assertThat(sheets.unseal(new ByteArrayInputStream(b))).isEqualTo(table);
	}
}
