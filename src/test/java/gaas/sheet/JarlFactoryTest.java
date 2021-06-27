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
 * {@link JarlFactory}クラスのテスト用クラスです。
 *
 *
 * @author 無線部開発班
 *
 * @since 2017/03/12
 */
public final class JarlFactoryTest extends Assertions {
	private final SheetManager sheets = new SheetManager();
	private final TableManager tables = new TableManager();
	private final ArrayList<Band> bands = new ArrayList<>();

	public JarlFactoryTest() {
		bands.add(new Band(3_500));
		bands.add(new Band(7_000));
		bands.add(new Band(14_000));
		bands.add(new Band(144_000));
		bands.add(new Band(1_200_000));
		bands.add(new Band(5_600_000));
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
			item.set(new Mode(alnum(5)));
			item.set(new Mul1(alnum(7)));
			item.getRcvd().set(new RSTQ(randInt(600)));
			item.getRcvd().set(new Code(alnum(7)));
			item.getSent().set(new RSTQ(randInt(600)));
			item.getSent().set(new Code(alnum(7)));
			list.add(item);
		}
		final var buf = new StringWriter();
		final var bin = tables.factory("jarl").encode(list);
		final var enc = sheets.factory("jarl").encoder(buf);
		enc.set("CALLSIGN", "JA1ZLO");
		enc.set("COMMENTS", "Groovy");
		enc.set("LOGSHEET", bin);
		enc.encode();
		final var str = buf.toString();
		final var src = new StringReader(str);
		final var dec = sheets.factory("jarl").decoder(src);
		dec.decode();
		assertThat(dec.getString("CALLSIGN")).isEqualTo("JA1ZLO");
		assertThat(dec.getString("COMMENTS")).isEqualTo("Groovy");
		assertThat(new SheetOrTable().unpack(str)).isEqualTo(list);
	}

	public static final IntStream source() {
		return IntStream.range(0, 100);
	}
}
