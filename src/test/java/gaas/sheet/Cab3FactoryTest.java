/*******************************************************************************
 * Amateur Radio Operational Logging Library 'qxsl' since 2013 February 16th
 * License : GNU Lesser General Public License v3 (see LICENSE)
 * Author: Journal of Hamradio Informatics (http://pafelog.net)
*******************************************************************************/
package gaas.sheet;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.stream.IntStream;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import qxsl.draft.*;
import qxsl.model.Item;
import qxsl.sheet.SheetManager;
import qxsl.sheet.SheetOrTable;
import qxsl.table.TableManager;

import static qxsl.junit.RandomNumberParameterExtension.randInt;
import static qxsl.junit.RandomStringParameterExtension.alnum;

/**
 * {@link Cab3Factory}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2019/05/03
 */
public final class Cab3FactoryTest extends Assertions {
	private final SheetManager sheets = new SheetManager();
	private final TableManager tables = new TableManager();
	private final ArrayList<Band> bands = new ArrayList<>();

	public Cab3FactoryTest() {
		bands.add(new Band(1_800));
		bands.add(new Band(3_500));
		bands.add(new Band(7_000));
		bands.add(new Band(14_000));
		bands.add(new Band(21_000));
		bands.add(new Band(28_000));
		bands.add(new Band(50_000));
	}

	@ParameterizedTest
	@MethodSource("source")
	public void testDecode(int numItems) throws IOException {
		final var list = new ArrayList<Item>();
		for (int row = 0; row < numItems; row++) {
			final var item = new Item();
			item.set(Time.now().copyDropSecond());
			item.set(bands.get(randInt(bands.size())));
			item.set(new Call(alnum(13)));
			item.set(new Mode(alnum(2)));
			item.getRcvd().set(new RSTQ(randInt(600)));
			item.getRcvd().set(new Code(alnum(6)));
			item.getSent().set(new RSTQ(randInt(600)));
			item.getSent().set(new Code(alnum(6)));
			list.add(item);
		}
		final var buf = new StringWriter();
		final var bin = tables.factory("cqww").encode(list);
		final var enc = sheets.factory("cab3").encoder(buf);
		enc.set("CONTEST", "JIDX-CW");
		enc.set("CALLSIGN", "JA1ZLO");
		enc.set("QSO", bin);
		enc.encode();
		final var str = buf.toString();
		final var src = new StringReader(str);
		final var dec = sheets.factory("cab3").decoder(src);
		dec.decode();
		assertThat(dec.getString("CONTEST")).isEqualTo("JIDX-CW");
		assertThat(dec.getString("CALLSIGN")).isEqualTo("JA1ZLO");
		assertThat(new SheetOrTable().unpack(str)).isEqualTo(list);
	}

	public static final IntStream source() {
		return IntStream.range(0, 100);
	}
}
